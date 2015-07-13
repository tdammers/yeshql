{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
where

import Language.Haskell.TH
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Database.HDBC (toSql, run, ConnWrapper, IConnection)
import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import Control.Applicative ( (<$>) )

data ParsedQuery =
    ParsedQuery
        { pqQueryName :: String
        , pqQueryString :: String
        , pqParams :: [(String, String)]
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
        mainP :: P.Parsec String () ParsedQuery
        mainP = do
            P.string "--name:"
            qn <- P.manyTill P.anyChar (P.char '\n')
            items <- P.many itemP
            P.eof
            return $ ParsedQuery
                        qn
                        (extractParsedQuery items)
                        (extractParsedParams items)

        itemP :: P.Parsec String () ParsedItem
        itemP = paramP <|> quotedP <|> literalP

        paramP :: P.Parsec String () ParsedItem
        paramP = do
            P.char ':'
            pname <- P.many1 (P.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_"))
            ptype <- P.option "String" $ do
                        P.char ':'
                        P.many1 (P.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_"))
            return $ ParsedParam pname ptype

        quotedP :: P.Parsec String () ParsedItem
        quotedP = do
            P.char '\''
            contents <- P.many (P.noneOf "'")
            P.char '\''
            return . ParsedLiteral . ('\'':) . (++ "'") $ contents

        literalP :: P.Parsec String () ParsedItem
        literalP = ParsedLiteral <$> P.many1 (P.noneOf ":'")

mkQuery :: String -> String -> Q [Dec]
mkQuery funName q = do
    let items = words q
        params = catMaybes . map (\x -> if (":" `isPrefixOf` x) then (Just . drop 1 $ x) else Nothing) $ items
        items' = map (\x -> if (":" `isPrefixOf` x) then "?" else x) items
        q' = unwords items'
        patterns = (varP . mkName $ "conn") : map (varP . mkName) params
        body = (varE 'run)
                `appE` (varE . mkName $ "conn")
                `appE` (litE . stringL $ q')
                `appE` (listE [ appE (varE 'toSql) (varE . mkName $ n) | n <- params ])
    s <- sigD (mkName funName) [t| IConnection conn => conn -> String -> IO Integer |]
    f <- funD (mkName funName)
            [ clause
                (varP (mkName "conn"):map (varP . mkName) params)
                (normalB body)
                []
            ]
    return [s, f]
