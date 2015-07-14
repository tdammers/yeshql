{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
( yesh
, mkQuery
, parseQuery
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List (isPrefixOf, nub, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import Database.HDBC (fromSql, toSql, run, ConnWrapper, IConnection, quickQuery')
import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import Control.Applicative ( (<$>), (<*>) )
import Data.Char (chr, ord)
import qualified Data.Map.Strict as Map
import Data.Map (Map)

data ParsedQuery =
    ParsedQuery
        { pqQueryName :: String
        , pqQueryString :: String
        , pqParamsRaw :: [(String, String)]
        , pqParamNames :: [String]
        , pqParamTypes :: Map String String
        , pqReturnType :: Either String [String]
        }
        deriving (Show)

parsedQuery :: String -> String -> [(String, String)] -> [(String, String)] -> Either String [String] -> ParsedQuery
parsedQuery queryName queryString paramsRaw paramsExtra returnType =
    ParsedQuery
        queryName
        queryString
        paramsRaw
        (extractParamNames (paramsRaw ++ paramsExtra))
        (extractParamTypeMap (paramsRaw ++ paramsExtra))
        returnType

extractParamNames :: [(String, String)] -> [String]
extractParamNames = nub . map fst

extractParamTypeMap :: [(String, String)] -> Map String String
extractParamTypeMap = foldl' applyItem Map.empty
    where
        applyItem :: Map String String -> (String, String) -> Map String String
        applyItem m (n, t) =
            let tc = Map.lookup n m
            in case (tc, t) of
                (Nothing, "") -> m
                (Nothing, t) -> Map.insert n t m
                (Just "", "") -> m
                (Just "", t) -> Map.insert n t m
                (Just t', "") -> m
                (Just t', t) -> error $ "Inconsistent types found for parameter '" ++ n ++ "': '" ++ t' ++ "' vs. '" ++ t ++ "'"

data ParsedItem = ParsedLiteral String | ParsedParam String String | ParsedComment String

extractParsedQuery :: [ParsedItem] -> String
extractParsedQuery = concat . map extractItem
    where
        extractItem :: ParsedItem -> String
        extractItem (ParsedLiteral str) = str
        extractItem (ParsedParam _ _) = "?"

extractParsedParams :: [ParsedItem] -> [(String, String)]
extractParsedParams = catMaybes . map extractItem
    where
        extractItem :: ParsedItem -> Maybe (String, String)
        extractItem (ParsedLiteral _) = Nothing
        extractItem (ParsedParam n t) = Just (n, t)

parseQuery :: String -> Either P.ParseError ParsedQuery
parseQuery src = P.runParser mainP () "query" src
    where
        mainP :: P.Parsec String () ParsedQuery
        mainP = do
            (qn, retType) <- nameDeclP
            extraItems <- P.many (P.try paramDeclP)
            items <- P.many itemP
            P.eof
            return $ parsedQuery
                        qn
                        (extractParsedQuery items)
                        (extractParsedParams items)
                        (extractParsedParams extraItems)
                        retType

        nameDeclP :: P.Parsec String () (String, Either String [String])
        nameDeclP = do
            P.manyTill P.anyChar (P.try (P.string "--" >> P.spaces >> P.string "name" >> P.spaces >> P.char ':'))
            P.spaces
            qn <- identifierP
            P.spaces
            retType <- P.option (Left "Integer") (P.try (P.string "->") >> P.spaces >> returnTypeP)
            P.spaces
            return (qn, retType)

        identifierP :: P.Parsec String () String
        identifierP =
            (:) <$> leadCharP <*> P.many tailCharP
            where
                leadCharP = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
                tailCharP = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

        returnTypeP :: P.Parsec String () (Either String [String])
        returnTypeP = returnTypeMultiP <|> returnTypeSingleP

        returnTypeSingleP :: P.Parsec String () (Either String [String])
        returnTypeSingleP = Left <$> identifierP

        returnTypeMultiP :: P.Parsec String () (Either String [String])
        returnTypeMultiP =
            Right <$> P.between
                (P.char '(' >> P.spaces)
                (P.char ')' >> P.spaces)
                (P.sepBy (P.between P.spaces P.spaces identifierP) (P.char ','))

        itemP :: P.Parsec String () ParsedItem
        itemP = paramP <|> quotedP <|> literalP

        paramDeclP :: P.Parsec String () ParsedItem
        paramDeclP = do
            P.try $ (P.string "--" >> P.spaces >> P.char ':')
            name <- identifierP
            P.spaces
            t <- P.option "" $ do
                    P.char ':'
                    P.spaces
                    t <- identifierP
                    P.spaces
                    return t
            return $ ParsedParam name t

        commentP :: P.Parsec String () ParsedItem
        commentP = do
            P.string "--"
            P.spaces
            ParsedComment <$> P.manyTill P.anyChar (P.char '\n')

        paramP :: P.Parsec String () ParsedItem
        paramP = do
            P.char ':'
            pname <- identifierP
            ptype <- P.option "" $ do
                        P.char ':'
                        identifierP
            return $ ParsedParam pname ptype

        quotedP :: P.Parsec String () ParsedItem
        quotedP = do
            P.char '\''
            contents <- P.many (P.noneOf "'")
            P.char '\''
            return . ParsedLiteral . ('\'':) . (++ "'") $ contents

        literalP :: P.Parsec String () ParsedItem
        literalP = ParsedLiteral <$> P.many1 (P.noneOf ":'")

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
        argTypesStr = map (fromMaybe "String" . flip Map.lookup (pqParamTypes query)) $ argNamesStr
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
