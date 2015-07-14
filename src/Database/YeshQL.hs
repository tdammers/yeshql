{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
( yesh
, mkQueryDecs
, parseQuery
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List (isPrefixOf, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Database.HDBC (fromSql, toSql, run, ConnWrapper, IConnection, quickQuery')
import qualified Text.Parsec as P
import Data.Char (chr, ord, toUpper)
import Control.Applicative ( (<$>), (<*>) )

import Database.YeshQL.Parser

nthIdent :: Int -> String
nthIdent i
    | i < 26 = [chr (ord 'a' + i)]
    | otherwise = let (j, k) = divMod i 26
                    in nthIdent j ++ nthIdent k

yesh :: QuasiQuoter
yesh = QuasiQuoter
        { quoteDec = mkQueryDecs
        }

queryName :: String -> String -> Name
queryName prefix basename =
    mkName $ prefix ++ ucfirst basename

ucfirst :: String -> String
ucfirst "" = ""
ucfirst (x:xs) = toUpper x:xs

mkQueryDecs :: String -> Q [Dec]
mkQueryDecs qstr = do
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
    sRun <- sigD (queryName "run" funName) [t| IConnection conn => conn -> $(queryType) |]
    fRun <- funD (queryName "run" funName)
                [ clause
                    (varP (mkName "conn"):map varP argNames)
                    (normalB body)
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
