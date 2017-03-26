{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
module Database.YeshQL.SqlRow.Class
where

import Database.HDBC
import Data.Convertible (Convertible)

class ToSqlRow a where
    toSqlRow :: a -> [SqlValue]

class FromSqlRow a where
    parseSqlRow :: [SqlValue] -> Either String (a, [SqlValue])


fromSqlRow :: (FromSqlRow a, Monad m) => [SqlValue] -> m a
fromSqlRow sqlValues =
    case parseSqlRow sqlValues of
        Left err -> fail err
        Right (value, _) -> return value

class (ToSqlRow a, FromSqlRow a) => SqlRow a where
