{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Database.PostgreSQL.Simple
import qualified Database.YeshQL.PostgreSQL.Tests as Tests
import System.Environment
import System.Exit
import System.IO
import Test.Tasty

main :: IO ()
main = do
  mConnString <- lookupEnv "YESHQL_POSTGRESQL_CONNSTRING"
  case mConnString of
    Nothing -> do
      hPutStrLn stderr "Please set the 'YESHQL_POSTGRESQL_CONNSTRING' to the PostgreSQL connection string"
      exitFailure
    Just connString -> withConn (ByteString.pack connString) (defaultMain . allTests)
  where
    allTests conn = testGroup "All Tests" (Tests.tests conn)

withConn :: ByteString -> (Connection -> IO a) -> IO a
withConn connString f =
  bracket (connectPostgreSQL connString) close $ \c ->
  bracket_ (begin c) (rollback c) $ do
    initializeDb c
    f c

initializeDb :: Connection -> IO ()
initializeDb c = do
  execute_ c "create table users (id serial primary key, username text, person_name text, email text);"
  execute_ c "insert into users (username, person_name, email) values ('billy', 'Billy', 'billy@example.org');"
  pure ()
