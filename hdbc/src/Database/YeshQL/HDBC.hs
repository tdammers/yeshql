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
import Database.YeshQL.Backend
import Database.YeshQL.HDBC.SqlRow.Class

yesh :: Yesh a => a
yesh = yeshWith hdbcBackend

yesh1 :: Yesh a => a
yesh1 = yesh1With hdbcBackend

yeshFile :: YeshFile a => a
yeshFile = yeshFileWith hdbcBackend

yesh1File :: YeshFile a => a
yesh1File = yesh1FileWith hdbcBackend

hdbcBackend :: YeshBackend
hdbcBackend =
  YeshBackend
    { ybNames = pqNames
    , ybMkQueryBody = mkQueryBody
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

pqNames :: ParsedQuery -> ([Name], [PatQ], String, TypeQ)
pqNames query =
    let argNamesStr = pqParamNames query ++ ["conn"]
        argNames = map mkName argNamesStr
        patterns = map varP argNames
        funName = pqQueryName query
        queryType = pgQueryType query
    in
      (argNames, patterns, funName, queryType)

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
