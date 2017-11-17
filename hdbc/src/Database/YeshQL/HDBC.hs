{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE CPP #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleInstances #-}
{-|
Module: Database.YeshQL.HDBC
Description: Turn SQL queries into type-safe functions.
Copyright: (c) 2015-2017 Tobias Dammers
Maintainer: Tobias Dammers <tdammers@gmail.com>
Stability: experimental
License: MIT

 -}
module Database.YeshQL.HDBC
(
-- * Quasi-quoters that take strings
  Yesh (..)
-- * Quasi-quoters that take filenames
, YeshFile (..)
-- * Low-level generators in the 'Q' monad
, mkQueryDecs
, mkQueryExp
-- * Query parsers
, parseQuery
, parseQueries
-- * AST
, ParsedQuery (..)
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
#if MIN_VERSION_template_haskell(2,7,0)
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile))
#endif
import Data.List (isPrefixOf, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Database.HDBC (fromSql, toSql, run, runRaw, ConnWrapper, IConnection, quickQuery')
import qualified Text.Parsec as P
import Data.Char (chr, ord, toUpper, toLower)
import Control.Applicative ( (<$>), (<*>) )
import System.FilePath (takeBaseName)
import Data.Char (isAlpha, isAlphaNum)

import Database.YeshQL.Parser
import Database.YeshQL.Util
import Database.YeshQL.HDBC.SqlRow.Class

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

nthIdent :: Int -> String
nthIdent i
    | i < 26 = [chr (ord 'a' + i)]
    | otherwise = let (j, k) = divMod i 26
                    in nthIdent j ++ nthIdent k

-- | This typeclass is needed to allow 'yesh' and 'yesh1' to be used
-- interchangeably as quasi-quoters and TH functions.  Because the intended
-- instances are @String -> Q [Dec]@ and @QuasiQuoter@, it is unfortunately not
-- possible to give the methods more obvious signatures like @String -> a@.
class Yesh a where
    -- | Generate top-level declarations or expressions for several SQL
    -- queries.  If used at the top level (i.e., generating declarations), all
    -- queries in the definitions must be named, and 'yesh' will generate a
    -- separate set of functions for each.  If used in an expression context,
    -- the current behavior is somewhat undesirable, namely sequencing the
    -- queries using '>>'.
    --
    -- Future versions will most likely change this to create a tuple of query
    -- expressions instead, such that you can write something like:
    --
    -- @
    -- let (createUser, getUser, updateUser, deleteUser) = [yesh|
    --      -- name:createUser :: (Integer)
    --      -- :username :: String
    --      INSERT INTO users (username) VALUES (:username) RETURNING id;
    --      -- name:getUser :: (Integer, String)
    --      -- :userID :: Integer
    --      SELECT id, username FROM users WHERE id = :userID;
    --      -- name:updateUser :: Integer
    --      -- :userID :: Integer
    --      -- :username :: String
    --      UPDATE users SET username = :username WHERE id = :userID;
    --      -- name:deleteUser :: Integer
    --      -- :userID :: Integer
    --      DELETE FROM users WHERE id = :userID LIMIT 1;
    --  |]
    -- @

    yesh :: a
    -- | Generate a top-level declaration or an expression for a single SQL
    -- query.  If used at the top level (i.e., generating a declaration), the
    -- query definition must specify a query name.
    yesh1 :: a

-- | This typeclass is needed to allow 'yeshFile' and 'yesh1File' to be used
-- interchangeably as quasi-quoters and TH functions.
-- Because the intended instances are @FilePath -> Q [Dec]@ and @QuasiQuoter@,
-- it is unfortunately not possible to give the methods more obvious signatures
-- like @FilePath -> a@.
class YeshFile a where
    -- | Generate multiple query definitions or expressions from an external
    -- file.  Query name derivation works exactly like for 'yesh1File', except
    -- that an underscore and a 0-based query index are appended to
    -- disambiguate queries from the same file.
    --
    -- In an expression context, the same caveats apply as for 'yesh', i.e., to
    -- generate expressions, you will almost certainly want 'yesh1File', not
    -- 'yeshFile'.
    yeshFile :: a

    -- | Generate one query definition or expression from an external file.  In
    -- a declaration context, the query name will be derived from the filename
    -- unless the query contains an explicit name. Query name derivation works
    -- as follows:
    --
    -- 1. Take only the basename (stripping off the directories and extension)
    -- 2. Remove all non-alphabetic characters from the beginning of the name
    -- 3. Remove all non-alphanumeric characters from the name
    -- 4. Lower-case the first character.
    --
    -- Note that since there is always a filename to derive the query name
    -- from, explicitly defining a query name is only necessary when you want
    -- it to differ from the filename; however, making it explicit anyway is
    -- probably a good idea.
    yesh1File :: a

instance Yesh (String -> Q [Dec]) where
    yesh1 = withParsedQuery mkQueryDecs
    yesh = withParsedQueries mkQueryDecsMulti

instance YeshFile (FilePath -> Q [Dec]) where
    yesh1File = withParsedQueryFile mkQueryDecs
    yeshFile = withParsedQueriesFile mkQueryDecsMulti

instance Yesh (String -> Q Exp) where
    yesh1 = withParsedQuery mkQueryExp
    yesh = withParsedQueries mkQueryExpMulti

instance YeshFile (FilePath -> Q Exp) where
    yesh1File = withParsedQueryFile mkQueryExp
    yeshFile = withParsedQueriesFile mkQueryExpMulti

instance Yesh QuasiQuoter where
    yesh = QuasiQuoter
            { quoteDec = yesh
            , quoteExp = yesh
            , quoteType = error "YeshQL does not generate types"
            , quotePat = error "YeshQL does not generate patterns"
            }
    yesh1 = QuasiQuoter
            { quoteDec = yesh1
            , quoteExp = yesh1
            , quoteType = error "YeshQL does not generate types"
            , quotePat = error "YeshQL does not generate patterns"
            }

instance YeshFile QuasiQuoter where
    yeshFile = QuasiQuoter
            { quoteDec = yeshFile
            , quoteExp = yeshFile
            , quoteType = error "YeshFileQL does not generate types"
            , quotePat = error "YeshFileQL does not generate patterns"
            }
    yesh1File = QuasiQuoter
            { quoteDec = yesh1File
            , quoteExp = yesh1File
            , quoteType = error "YeshFileQL does not generate types"
            , quotePat = error "YeshFileQL does not generate patterns"
            }

pgQueryType :: ParsedQuery -> TypeQ
pgQueryType query =
    [t|forall conn. IConnection conn =>
        $(foldr
            (\a b -> [t| $a -> $b |])
            [t| conn -> IO $(returnType) |]
          $ argTypes)
      |]
    where
        argTypes = map (mkType . fromMaybe AutoType . pqTypeFor query) (pqParamNames query)
        returnType =
            if pqDDL query
                then
                    tupleT 0
                else
                    case pqReturnType query of
                        ReturnRowCount tn -> mkType tn
                        ReturnTuple One [] -> tupleT 0
                        ReturnTuple One (x:[]) -> appT [t|Maybe|] $ mkType x
                        ReturnTuple One xs -> appT [t|Maybe|] $ foldl' appT (tupleT $ length xs) (map mkType xs)
                        ReturnTuple Many [] -> tupleT 0
                        ReturnTuple Many (x:[]) -> appT listT $ mkType x
                        ReturnTuple Many xs -> appT listT $ foldl' appT (tupleT $ length xs) (map mkType xs)
                        ReturnRecord One x -> appT [t|Maybe|] $ mkType x
                        ReturnRecord Many x -> appT listT $ mkType x

mkType :: ParsedType -> Q Type
mkType (MaybeType n) = [t|Maybe $(conT . mkName $ n)|]
mkType (PlainType n) = conT . mkName $ n
mkType AutoType = [t|String|]

mkQueryDecsMulti :: [ParsedQuery] -> Q [Dec]
mkQueryDecsMulti queries = concat <$> mapM mkQueryDecs queries

-- TODO: instead of sequencing the queries, put them in a tuple.
mkQueryExpMulti :: [ParsedQuery] -> Q Exp
mkQueryExpMulti queries =
    foldl1 (\a b -> VarE '(>>) `AppE` a `AppE` b) <$> mapM mkQueryExp queries

pqNames :: ParsedQuery -> ([Name], [PatQ], String, TypeQ)
pqNames query =
    let argNamesStr = pqParamNames query ++ ["conn"]
        argNames = map mkName argNamesStr
        patterns = map varP argNames
        funName = pqQueryName query
        queryType = pgQueryType query
    in (argNames, patterns, funName, queryType)

mkQueryDecs :: ParsedQuery -> Q [Dec]
mkQueryDecs query = do
    let (argNames, patterns, funName, queryType) = pqNames query
    sRun <- sigD (mkName . lcfirst $ funName) queryType
    fRun <- funD (mkName . lcfirst $ funName)
                [ clause
                    (map varP argNames)
                    (normalB . mkQueryBody $ query)
                    []
                ]
    sDescribe <- sigD (queryName "describe" funName) [t|String|]
    fDescribe <- funD (queryName "describe" funName)
                    [ clause
                        []
                        (normalB . litE . stringL . pqQueryString $ query)
                        []
                    ]
    sDocument <- sigD (queryName "doc" funName) [t|String|]
    fDocument <- funD (queryName "doc" funName)
                    [ clause
                        []
                        (normalB . litE . stringL . pqDocComment $ query)
                        []
                    ]
    return [sRun, fRun, sDescribe, fDescribe, sDocument, fDocument]

mkQueryExp :: ParsedQuery -> Q Exp
mkQueryExp query = do
    let (argNames, patterns, funName, queryType) = pqNames query
    sigE
        (lamE patterns (mkQueryBody query))
        queryType

mkQueryBody :: ParsedQuery -> Q Exp
mkQueryBody query = do
    let (argNames, patterns, funName, queryType) = pqNames query

        convert :: ExpQ
        convert = case pqReturnType query of
                    ReturnRowCount tn -> varE 'fromInteger
                    ReturnTuple _ [] -> [|\_ -> ()|]
                    ReturnTuple _ (x:[]) -> [|map (fromSql . head)|]
                    ReturnTuple _ xs ->
                        let varNames = map nthIdent [0..pred (length xs)]
                        in [|map $(lamE
                                    -- \[a,b,c,...] ->
                                    [(listP (map (varP . mkName) varNames))]
                                    -- (fromSql a, fromSql b, fromSql c, ...)
                                    (tupE $ (map (\n -> appE (varE 'fromSql) (varE . mkName $ n)) varNames)))|]
                    ReturnRecord _ x -> [|fromSqlRow|]
        queryFunc = case pqReturnType query of
                        ReturnRowCount _ ->
                            [| \qstr params conn -> $convert <$> run conn qstr params |]
                        ReturnTuple Many _ ->
                            [| \qstr params conn -> $convert <$> quickQuery' conn qstr params |]
                        ReturnTuple One _ ->
                            [| \qstr params conn -> fmap headMay $ $convert <$> quickQuery' conn qstr params |]
                        ReturnRecord Many _ ->
                            [| \qstr params conn -> mapM $convert =<< quickQuery' conn qstr params |]
                        ReturnRecord One _ ->
                            [| \qstr params conn -> fmap headMay $ mapM $convert =<< quickQuery' conn qstr params |]
        rawQueryFunc = [| \qstr conn -> runRaw conn qstr |]
    if pqDDL query
        then
            rawQueryFunc
                `appE` (litE . stringL . pqQueryString $ query)
                `appE` (varE . mkName $ "conn")
        else
            queryFunc
                `appE` (litE . stringL . pqQueryString $ query)
                `appE` (listE (map paramArg $ pqParamsRaw query))
                `appE` (varE . mkName $ "conn")

    where
        paramArg :: ExtractedParam -> ExpQ
        paramArg (ExtractedParam n ps _) = do
            let valE = foldl1 (flip appE) (map (varE . mkName) (n:ps))
            varE 'toSql `appE` valE
