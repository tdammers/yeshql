{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
module Database.YeshQL.SqlEntity
where

import Database.HDBC
import Data.Convertible (Convertible)

class SqlEntity a where
    toSqlRow :: a -> [SqlValue]
    fromSqlRow :: Monad m => [SqlValue] -> m a
