module Database.YeshQL.Parser
( parseQuery
, parseQueries
, parseQueryN
, parseQueriesN
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
        (extractParamNames (paramsExtra ++ paramsRaw))
        (extractParamTypeMap (paramsExtra ++ paramsRaw))
        returnType
        docComment

extractParamNames :: [(String, ParsedType)] -> [String]
extractParamNames xs = 
    nub . map fst $ xs

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

parseQueryN :: String -> String -> Either ParseError ParsedQuery
parseQueryN fn src = 
    runParser mainP () fn src

parseQuery :: String -> Either ParseError ParsedQuery
parseQuery = parseQueryN ""

parseQueriesN :: String -> String -> Either ParseError [ParsedQuery]
parseQueriesN fn src =
    runParser multiP () fn src

parseQueries :: String -> Either ParseError [ParsedQuery]
parseQueries = parseQueriesN ""

mainP :: Parsec String () ParsedQuery
mainP = do
    q <- queryP
    eof
    return q

multiP :: Parsec String () [ParsedQuery]
multiP = do
    fmap catMaybes $
        sepBy queryMayP sepP
    where
        sepP :: Parsec String () ()
        sepP = try (spaces >> char ';' >> notFollowedBy (char ';') >> spaces)
        queryMayP :: Parsec String () (Maybe ParsedQuery)
        queryMayP = do
            spaces
            fmap Just queryP <|> (eof >> return Nothing) <?> "SQL query"

queryP :: Parsec String () ParsedQuery
queryP = do
    spaces
    (qn, retType) <- option ("", Left (PlainType "Integer")) $ nameDeclP <|> namelessDeclP
    extraItems <- many (paramDeclP <|> commentP)
    items <- many (try commentP <|> try itemP)
    return $ parsedQuery
                qn
                (extractParsedQuery items)
                (extractParsedParams items)
                (extractParsedParams extraItems)
                retType
                (extractDocComment (extraItems ++ items))

nameDeclP :: Parsec String () (String, Either ParsedType [ParsedType])
nameDeclP = do
    try (whitespaceP >> string "--" >> whitespaceP >> string "name" >> whitespaceP >> char ':')
    whitespaceP
    qn <- identifierP
    whitespaceP
    retType <- option (Left (PlainType "Integer")) (try (string "::" >> whitespaceP >> returnTypeP))
    whitespaceP
    newlineP
    return (qn, retType)

namelessDeclP :: Parsec String () (String, Either ParsedType [ParsedType])
namelessDeclP = do
    try (whitespaceP >> string "--" >> whitespaceP >> string "::" >> whitespaceP)
    retType <- returnTypeP
    whitespaceP
    newlineP
    return ("", retType)

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
            string "::"
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
    ParsedComment <$> manyTill (noneOf " \t\n;") (newlineP <|> ignore (char ';'))

paramP :: Parsec String () ParsedItem
paramP = do
    char ':'
    pname <- identifierP
    ptype <- option AutoType $ do
                string "::"
                typeP
    return $ ParsedParam pname ptype

quotedP :: Parsec String () ParsedItem
quotedP = do
    char '\''
    contents <- many (noneOf "'")
    char '\''
    return . ParsedLiteral . ('\'':) . (++ "'") $ contents

literalP :: Parsec String () ParsedItem
literalP = ParsedLiteral <$> many1 (noneOf ":';")

whitespaceP :: Parsec String () ()
whitespaceP = do
    many (oneOf " \t\r")
    return ()

ignore :: Parsec s u a -> Parsec s u ()
ignore x = x >> return ()

newlineP :: Parsec String () ()
newlineP = do
    (ignore $ char '\n') <|> (ignore . try . string $ ";;")
    return ()
