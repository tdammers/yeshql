{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE CPP #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleInstances #-}
{-|
Module: Database.YeshQL.PostgreSQL
Description: Turn SQL queries into type-safe functions.
Copyright: (c) 2015-2017 Tobias Dammers and contributors
Maintainer: Tobias Dammers <tdammers@gmail.com>
Stability: experimental
License: MIT

 -}
module Database.YeshQL.PostgreSQL
(
-- * Quasi-quoters that take strings
  yesh, yesh1
-- * Quasi-quoters that take filenames
, yeshFile, yesh1File
-- * Query parsers
, parseQuery
, parseQueries
-- * AST
, ParsedQuery (..)
)
where

import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
#if MIN_VERSION_template_haskell(2,7,0)
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile))
#endif
import Data.List (isPrefixOf, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import qualified Text.Parsec as P
import Data.Char (chr, ord, toUpper, toLower)
import Control.Applicative ( (<$>), (<*>) )
import System.FilePath (takeBaseName)
import Data.Char (isAlpha, isAlphaNum)

import qualified Database.PostgreSQL.Simple as PostgreSQL

import Database.YeshQL.Parser
import Database.YeshQL.Util
import Database.YeshQL.Backend

yesh :: Yesh a => a
yesh = yeshWith pgBackend

yesh1 :: Yesh a => a
yesh1 = yesh1With pgBackend

yeshFile :: YeshFile a => a
yeshFile = yeshFileWith pgBackend

yesh1File :: YeshFile a => a
yesh1File = yesh1FileWith pgBackend

pgBackend :: YeshBackend
pgBackend =
  YeshBackend
    { ybNames = pqNames
    , ybMkQueryBody = mkQueryBody
    }

pgQueryType :: ParsedQuery -> TypeQ
pgQueryType query =
    [t|PostgreSQL.Connection ->
        $(foldr
            (\a b -> [t| $a -> $b |])
            [t| IO $(returnType) |]
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

pqNames :: ParsedQuery -> ([Name], [PatQ], String, TypeQ)
pqNames query =
    let argNamesStr = "conn" : pqParamNames query
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
                    ReturnTuple _ (x:[]) -> [|id|]
                    ReturnTuple _ xs -> [|id|]
                    ReturnRecord _ x -> [|id|]
        queryFunc = case pqReturnType query of
                        ReturnRowCount _ ->
                            [| \qstr params conn -> fmap fromIntegral (PostgreSQL.execute conn (fromString qstr) params) |]
                        ReturnTuple Many tys ->
                            case tys of
                             [t] -> [| \qstr params conn -> _ $ PostgreSQL.query conn (fromString qstr) params |]
                             _ -> [| \qstr params conn -> PostgreSQL.query conn (fromString qstr) params |]
                        ReturnTuple One tys ->
                            case tys of
                              [t] -> [| \qstr params conn -> fmap (fmap fromOnly . headMay) (PostgreSQL.query conn (fromString qstr) params) |]
                              _ -> [| \qstr params conn -> fmap headMay (PostgreSQL.query conn (fromString qstr) params) |]
                        ReturnRecord Many _ ->
                            [| \qstr params conn -> _ |]
                        ReturnRecord One _ ->
                            [| \qstr params conn -> fmap headMay (PostgreSQL.query conn (fromString qstr) params) |]
        rawQueryFunc = [| \qstr conn -> () <$ execute_ conn (fromString qstr) |]
    if pqDDL query
        then
            rawQueryFunc
                `appE` (litE . stringL . pqQueryString $ query)
                `appE` (varE . mkName $ "conn")
        else
            queryFunc
                `appE` (litE . stringL . pqQueryString $ query)
                `appE` (case map paramArg $ pqParamsRaw query of
                          [] -> [| () |]
                          [p] -> conE 'PostgreSQL.Only `appE` p
                          ps -> tupE ps)
                `appE` (varE . mkName $ "conn")

    where
        paramArg :: ExtractedParam -> ExpQ
        paramArg (ExtractedParam n ps _) =
            foldl1 (flip appE) (map (varE . mkName) (n:ps))
