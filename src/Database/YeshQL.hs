{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
( yesh
, mkQuery
, parseQuery
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List (isPrefixOf, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Database.HDBC (fromSql, toSql, run, ConnWrapper, IConnection, quickQuery')
import qualified Text.Parsec as P
import Data.Char (chr, ord)
import Control.Applicative ( (<$>), (<*>) )

import Database.YeshQL.Parser

nthIdent :: Int -> String
nthIdent i
    | i < 26 = [chr (ord 'a' + i)]
    | otherwise = let (j, k) = divMod i 26
                    in nthIdent j ++ nthIdent k

yesh :: QuasiQuoter
yesh = QuasiQuoter { quoteDec = mkQuery }

mkQuery :: String -> Q [Dec]
mkQuery qstr = do
    let parseResult = parseQuery qstr
    query <- case parseResult of
                Left err -> fail . show $ err
                Right x -> return x
    let argNamesStr = pqParamNames query
        argTypesStr = map (fromMaybe "String" . pqTypeFor query) $ argNamesStr
        argNames = map mkName argNamesStr
        argTypes = map mkName argTypesStr
        patterns = (varP . mkName $ "conn") : map varP argNames
        funName = pqQueryName query
        returnType = case pqReturnType query of
                        Left tn -> conT . mkName $ tn
                        Right [] -> tupleT 0
                        Right (x:[]) -> conT . mkName $ x
                        Right xs -> appT listT $ foldl' appT (tupleT $ length xs) (map (conT . mkName) xs)
        convert :: ExpQ
        convert = case pqReturnType query of
                    Left tn -> varE 'id
                    Right [] -> [|\_ -> ()|]
                    Right (x:[]) -> [|map (toSql . head)|]
                    Right xs ->
                        let varNames = map nthIdent [0..pred (length xs)]
                        in [|map $(lamE
                                    -- \[a,b,c,...] ->
                                    [(listP (map (varP . mkName) varNames))]
                                    -- (fromSql a, fromSql b, fromSql c, ...)
                                    (tupE $ (map (\n -> appE (varE 'fromSql) (varE . mkName $ n)) varNames)))|]
        queryType :: TypeQ
        queryType = foldr (\a b -> [t| $a -> $b |]) [t| IO $(returnType) |] $ map conT argTypes
        queryFunc = case pqReturnType query of
                        Left _ -> varE 'run
                        Right _ -> [| \conn qstr params -> $(convert) <$> quickQuery' conn qstr params |]
        body = queryFunc
                `appE` (varE . mkName $ "conn")
                `appE` (litE . stringL . pqQueryString $ query)
                `appE` (listE [ appE (varE 'toSql) (varE . mkName $ n) | (n, t) <- (pqParamsRaw query) ])
    s <- sigD (mkName funName) [t| IConnection conn => conn -> $(queryType) |]
    f <- funD (mkName funName)
            [ clause
                (varP (mkName "conn"):map varP argNames)
                (normalB body)
                []
            ]
    return [s, f]
