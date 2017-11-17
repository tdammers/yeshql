{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
module Database.YeshQL.HDBC.SqlRow.Class
where

import Database.HDBC
import Data.Convertible (Convertible, prettyConvertError)

class ToSqlRow a where
    toSqlRow :: a -> [SqlValue]

newtype Parser a =
    Parser { runParser :: [SqlValue] -> Either String (a, [SqlValue]) }
    deriving (Functor)

instance Applicative Parser where
    pure x = Parser $ \values -> Right (x, values)
    (<*>) = parserApply

parserApply :: Parser (a -> b) -> Parser a -> Parser b
parserApply (Parser rpf) (Parser rpa) =
    Parser $ \values ->
        case rpf values of
            Left err -> Left err
            Right (f, values') ->
                case rpa values' of
                    Left err -> Left err
                    Right (a, values'') ->
                        Right (f a, values'')

instance Monad Parser where
    (>>=) = parserBind
    fail err = Parser . const . Left $ err

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind p f =
    let g = runParser p
    in Parser $ \values -> case g values of
        Left err -> Left err
        Right (x, values') -> runParser (f x) values'

class FromSqlRow a where
    parseSqlRow :: Parser a

fromSqlRow :: (FromSqlRow a, Monad m) => [SqlValue] -> m a
fromSqlRow sqlValues =
    case runParser parseSqlRow sqlValues of
        Left err -> fail err
        Right (value, _) -> return value

class (ToSqlRow a, FromSqlRow a) => SqlRow a where

parseField :: Convertible SqlValue a => Parser a
parseField = Parser $ \case
    [] -> Left "Not enough columns in result set"
    (x:xs) -> case safeFromSql x of
        Left cerr -> Left . prettyConvertError $ cerr
        Right a -> Right (a, xs)
