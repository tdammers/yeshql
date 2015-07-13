{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
module Main where

import Database.YeshQL
import Database.HDBC
import Database.HDBC.PostgreSQL

[yesh| -- name:testQ -> (Integer, String)
       SELECT id, name FROM items WHERE id = :id:Integer
|]

dsn :: String
dsn = "host=localhost dbname=scrap user=scrap password=scrap"

main :: IO ()
main = do
    withPostgreSQL dsn $ \conn -> do
        testQ conn 3 >>= print
