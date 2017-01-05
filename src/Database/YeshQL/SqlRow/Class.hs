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
    fromSqlRow :: Monad m => [SqlValue] -> m a

class (ToSqlRow a, FromSqlRow a) => SqlRow a where
