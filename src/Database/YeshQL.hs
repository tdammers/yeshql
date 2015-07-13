{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
( yesh
, mkQuery
)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Database.HDBC (fromSql, toSql, run, ConnWrapper, IConnection, quickQuery')
import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import Control.Applicative ( (<$>), (<*>) )
import Data.Char (chr, ord)

data ParsedQuery =
    ParsedQuery
        { pqQueryName :: String
        , pqQueryString :: String
        , pqParams :: [(String, String)]
        , pqReturnType :: Either String [String]
        }
        deriving (Show)

data ParsedItem = ParsedLiteral String | ParsedParam String String

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
        identifierP :: P.Parsec String () String
        identifierP =
            (:) <$> leadCharP <*> P.many tailCharP
            where
                leadCharP = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
                tailCharP = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

        mainP :: P.Parsec String () ParsedQuery
        mainP = do
            P.manyTill P.anyChar (P.try (P.string "--" >> P.spaces >> P.string "name" >> P.spaces >> P.char ':'))
            P.spaces
            qn <- identifierP
            P.spaces
            retType <- P.option (Left "Integer") (P.try (P.string "->") >> P.spaces >> returnTypeP)

            items <- P.many itemP
            P.eof
            return $ ParsedQuery
                        qn
                        (extractParsedQuery items)
                        (extractParsedParams items)
                        retType

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

        paramP :: P.Parsec String () ParsedItem
        paramP = do
            P.char ':'
            pname <- identifierP
            ptype <- P.option "String" $ do
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
    let patterns = (varP . mkName $ "conn") : map (varP . mkName . fst) (pqParams query)
        funName = pqQueryName query
        returnType = case pqReturnType query of
                        Left tn -> conT . mkName $ tn
                        Right [] -> tupleT 0
                        Right (x:[]) -> conT . mkName $ x
                        Right xs -> appT listT $ foldl appT (tupleT $ length xs) (map (conT . mkName) xs)
        convert :: ExpQ
        convert = case pqReturnType query of
                    Left tn -> varE 'id
                    Right [] -> [|\_ -> ()|]
                    Right (x:[]) -> [|map (toSql . head)|]
                    -- [[SqlValue]] -> [(Type0, Type1, ...)]
                    -- map ()
                    Right xs ->
                        let varNames = map nthIdent [0..pred (length xs)]
                        in [|map $(lamE
                                    -- \[a,b,c,...] ->
                                    [(listP (map (varP . mkName) varNames))]
                                    -- (fromSql a, fromSql b, fromSql c, ...)
                                    (tupE $ (map (\n -> appE (varE 'fromSql) (varE . mkName $ n)) varNames)))|]
        queryType :: TypeQ
        queryType = foldr (\a b -> [t| $a -> $b |]) [t| IO $(returnType) |] $ map (conT . mkName . snd) (pqParams query)
        queryFunc = case pqReturnType query of
                        Left _ -> varE 'run
                        Right _ -> [| \conn qstr params -> $(convert) <$> quickQuery' conn qstr params |]
        body = queryFunc
                `appE` (varE . mkName $ "conn")
                `appE` (litE . stringL . pqQueryString $ query)
                `appE` (listE [ appE (varE 'toSql) (varE . mkName $ n) | (n, t) <- (pqParams query) ])
    s <- sigD (mkName funName) [t| IConnection conn => conn -> $(queryType) |]
    f <- funD (mkName funName)
            [ clause
                (varP (mkName "conn"):map (varP . mkName . fst) (pqParams query))
                (normalB body)
                []
            ]
    return [s, f]
