{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
( yesh, yesh1
, mkQueryDecs
, mkQueryExp
, parseQuery
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List (isPrefixOf, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Database.HDBC (fromSql, toSql, run, ConnWrapper, IConnection, quickQuery')
import qualified Text.Parsec as P
import Data.Char (chr, ord, toUpper, toLower)
import Control.Applicative ( (<$>), (<*>) )

import Database.YeshQL.Parser

nthIdent :: Int -> String
nthIdent i
    | i < 26 = [chr (ord 'a' + i)]
    | otherwise = let (j, k) = divMod i 26
                    in nthIdent j ++ nthIdent k

yesh1 :: QuasiQuoter
yesh1 = QuasiQuoter
        { quoteDec = withParsedQuery mkQueryDecs
        , quoteExp = withParsedQuery mkQueryExp
        }

yesh :: QuasiQuoter
yesh = QuasiQuoter
        { quoteDec = withParsedQueries mkQueryDecsMulti
        , quoteExp = withParsedQueries mkQueryExpMulti
        }

queryName :: String -> String -> Name
queryName prefix basename =
    mkName $ prefix ++ ucfirst basename

ucfirst :: String -> String
ucfirst "" = ""
ucfirst (x:xs) = toUpper x:xs

lcfirst :: String -> String
lcfirst "" = ""
lcfirst (x:xs) = toLower x:xs

withParsedQuery :: (ParsedQuery -> Q a) -> String -> Q a
withParsedQuery = withParsed parseQuery

withParsedQueries :: ([ParsedQuery] -> Q a) -> String -> Q a
withParsedQueries = withParsed parseQueries

withParsed :: (Monad m, Show e) => (s -> Either e a) -> (a -> m b) -> s -> m b
withParsed p a src = do
    let parseResult = p src
    arg <- case parseResult of
                Left e -> fail . show $ e
                Right x -> return x
    a arg


pgQueryType :: ParsedQuery -> TypeQ
pgQueryType query = [t|IConnection conn => $(foldr (\a b -> [t| $a -> $b |]) [t| conn -> IO $(returnType) |] $ argTypes) |]
    where
        argTypes = map (mkType . fromMaybe AutoType . pqTypeFor query) (pqParamNames query)
        returnType = case pqReturnType query of
                        Left tn -> mkType tn
                        Right [] -> tupleT 0
                        Right (x:[]) -> appT listT $ mkType x
                        Right xs -> appT listT $ foldl' appT (tupleT $ length xs) (map mkType xs)

mkType :: ParsedType -> Q Type
mkType (MaybeType n) = [t|Maybe $(conT . mkName $ n)|]
mkType (PlainType n) = conT . mkName $ n
mkType AutoType = [t|String|]

mkQueryDecsMulti :: [ParsedQuery] -> Q [Dec]
mkQueryDecsMulti queries = concat <$> mapM mkQueryDecs queries

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
                    Left tn -> varE 'fromInteger
                    Right [] -> [|\_ -> ()|]
                    Right (x:[]) -> [|map (fromSql . head)|]
                    Right xs ->
                        let varNames = map nthIdent [0..pred (length xs)]
                        in [|map $(lamE
                                    -- \[a,b,c,...] ->
                                    [(listP (map (varP . mkName) varNames))]
                                    -- (fromSql a, fromSql b, fromSql c, ...)
                                    (tupE $ (map (\n -> appE (varE 'fromSql) (varE . mkName $ n)) varNames)))|]
        queryFunc = case pqReturnType query of
                        Left _ -> [| \qstr params conn -> $convert <$> run conn qstr params |]
                        Right _ -> [| \qstr params conn -> $convert <$> quickQuery' conn qstr params |]
    queryFunc
        `appE` (litE . stringL . pqQueryString $ query)
        `appE` (listE [ appE (varE 'toSql) (varE . mkName $ n) | (n, t) <- (pqParamsRaw query) ])
        `appE` (varE . mkName $ "conn")
