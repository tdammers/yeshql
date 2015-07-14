{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
module Main where

import Database.YeshQL
import Database.HDBC
import Database.HDBC.PostgreSQL

[yesh|
-- name:testQ -> (Integer, String)
-- :id:Integer
SELECT id, name FROM items WHERE id = :id OR id = :id + 1
|]

queryInfo =
    parseQuery . unlines $
        [ "-- name:testQ -> (Integer, String)"
        , "-- This is just a comment"
        , "-- :id:Integer"
        , "-- This is also just a comment"
        , "SELECT id, name FROM items WHERE id = :id OR id = :id + 1"
        ]

dsn :: String
dsn = "host=localhost dbname=scrap user=scrap password=scrap"

main :: IO ()
main = do
    print queryInfo
    withPostgreSQL dsn $ \conn -> do
        testQ conn 3 >>= print
