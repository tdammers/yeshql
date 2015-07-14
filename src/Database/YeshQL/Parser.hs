module Database.YeshQL.Parser
( parseQuery
, ParsedQuery (..)
, ParsedType (..)
, pqTypeFor
)
where

import Text.Parsec
import Control.Applicative ( (<$>), (<*>) )

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Data.List (foldl', nub)
import Data.Maybe (catMaybes, fromMaybe)

data ParsedType = PlainType String | MaybeType String | AutoType
    deriving Show

data ParsedQuery =
    ParsedQuery
        { pqQueryName :: String
        , pqQueryString :: String
        , pqParamsRaw :: [(String, ParsedType)]
        , pqParamNames :: [String]
        , pqParamTypes :: Map String ParsedType
        , pqReturnType :: Either ParsedType [ParsedType]
        , pqDocComment :: String
        }
        deriving (Show)

pqTypeFor :: ParsedQuery -> String -> Maybe ParsedType
pqTypeFor q pname = Map.lookup pname (pqParamTypes q)

parsedQuery :: String -> String -> [(String, ParsedType)] -> [(String, ParsedType)] -> Either ParsedType [ParsedType] -> String -> ParsedQuery
parsedQuery queryName queryString paramsRaw paramsExtra returnType docComment =
    ParsedQuery
        queryName
        queryString
        paramsRaw
        (extractParamNames (paramsRaw ++ paramsExtra))
        (extractParamTypeMap (paramsRaw ++ paramsExtra))
        returnType
        docComment

extractParamNames :: [(String, ParsedType)] -> [String]
extractParamNames = nub . map fst

extractParamTypeMap :: [(String, ParsedType)] -> Map String ParsedType
extractParamTypeMap = foldl' applyItem Map.empty
    where
        applyItem :: Map String ParsedType -> (String, ParsedType) -> Map String ParsedType
        applyItem m (n, t) =
            let tc = Map.lookup n m
            in case (tc, t) of
                (Nothing, AutoType) -> m
                (Nothing, t) -> Map.insert n t m
                (Just AutoType, AutoType) -> m
                (Just AutoType, t) -> Map.insert n t m
                (Just t', AutoType) -> m
                (Just t', t) -> error $ "Inconsistent types found for parameter '" ++ n ++ "': '" ++ show t' ++ "' vs. '" ++ show t ++ "'"

data ParsedItem = ParsedLiteral String | ParsedParam String ParsedType | ParsedComment String

extractParsedQuery :: [ParsedItem] -> String
extractParsedQuery = concat . map extractItem
    where
        extractItem :: ParsedItem -> String
        extractItem (ParsedLiteral str) = str
        extractItem (ParsedComment _) = ""
        extractItem (ParsedParam _ _) = "?"

extractParsedParams :: [ParsedItem] -> [(String, ParsedType)]
extractParsedParams = catMaybes . map extractItem
    where
        extractItem :: ParsedItem -> Maybe (String, ParsedType)
        extractItem (ParsedParam n t) = Just (n, t)
        extractItem _ = Nothing

extractDocComment :: [ParsedItem] -> String
extractDocComment = unlines . catMaybes . map extractItem
    where
        extractItem :: ParsedItem -> Maybe String
        extractItem (ParsedComment str) = Just str
        extractItem _ = Nothing

parseQuery :: String -> Either ParseError ParsedQuery
parseQuery src = runParser mainP () "query" src

mainP :: Parsec String () ParsedQuery
mainP = do
    (qn, retType) <- option ("query", Left (PlainType "Integer")) nameDeclP
    extraItems <- many (try paramDeclP <|> try commentP)
    items <- many (try commentP <|> try itemP)
    eof
    return $ parsedQuery
                qn
                (extractParsedQuery items)
                (extractParsedParams items)
                (extractParsedParams extraItems)
                retType
                (extractDocComment (extraItems ++ items))

nameDeclP :: Parsec String () (String, Either ParsedType [ParsedType])
nameDeclP = do
    manyTill anyChar (try (whitespaceP >> string "--" >> whitespaceP >> string "name" >> whitespaceP >> char ':'))
    whitespaceP
    qn <- identifierP
    whitespaceP
    retType <- option (Left (PlainType "Integer")) (try (string "->") >> whitespaceP >> returnTypeP)
    whitespaceP
    newlineP
    return (qn, retType)

identifierP :: Parsec String () String
identifierP =
    (:) <$> leadCharP <*> many tailCharP
    where
        leadCharP = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
        tailCharP = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

returnTypeP :: Parsec String () (Either ParsedType [ParsedType])
returnTypeP = returnTypeMultiP <|> returnTypeSingleP

returnTypeSingleP :: Parsec String () (Either ParsedType [ParsedType])
returnTypeSingleP = Left <$> typeP

returnTypeMultiP :: Parsec String () (Either ParsedType [ParsedType])
returnTypeMultiP =
    Right <$> between
        (char '(' >> whitespaceP)
        (char ')' >> whitespaceP)
        (sepBy (between whitespaceP whitespaceP typeP) (char ','))

typeP :: Parsec String () ParsedType
typeP = do
    name <- identifierP
    option (PlainType name) $ do
        char '?'
        return $ MaybeType name

itemP :: Parsec String () ParsedItem
itemP = paramP <|> quotedP <|> literalP

paramDeclP :: Parsec String () ParsedItem
paramDeclP = do
    try $ (whitespaceP >> string "--" >> whitespaceP >> char ':')
    name <- identifierP
    whitespaceP
    t <- option AutoType $ do
            char ':'
            whitespaceP
            t <- typeP
            whitespaceP
            return t
    newlineP
    return $ ParsedParam name t

commentP :: Parsec String () ParsedItem
commentP = do
    try (whitespaceP >> string "--")
    whitespaceP
    ParsedComment <$> manyTill anyChar newlineP

paramP :: Parsec String () ParsedItem
paramP = do
    char ':'
    pname <- identifierP
    ptype <- option AutoType $ do
                char ':'
                typeP
    return $ ParsedParam pname ptype

quotedP :: Parsec String () ParsedItem
quotedP = do
    char '\''
    contents <- many (noneOf "'")
    char '\''
    return . ParsedLiteral . ('\'':) . (++ "'") $ contents

literalP :: Parsec String () ParsedItem
literalP = ParsedLiteral <$> many1 (noneOf ":'")

whitespaceP :: Parsec String () ()
whitespaceP = do
    many (oneOf " \t\r")
    return ()

newlineP :: Parsec String () ()
newlineP = do
    char '\n' <|> char ';'
    return ()
