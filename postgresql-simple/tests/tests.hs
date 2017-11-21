{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Test.Tasty
import Database.PostgreSQL.Simple
import qualified Database.YeshQL.PostgreSQL.Tests as Tests

main :: IO ()
main = withConn (defaultMain . allTests)
  where
    allTests conn = testGroup "All Tests" (Tests.tests conn)

withConn :: (Connection -> IO a) -> IO a
withConn f =
  bracket (connectPostgreSQL "host='localhost' user='postgres'") close $ \c ->
  bracket_ (begin c) (rollback c) $ do
    initializeDb c
    f c

initializeDb :: Connection -> IO ()
initializeDb c = do
  execute_ c "create table users (id serial primary key, username text, person_name text, email text);"
  execute_ c "insert into users (username, person_name, email) values ('billy', 'Billy', 'billy@example.org');"
  pure ()
