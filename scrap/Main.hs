{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
module Main where

import Database.YeshQL
import Database.HDBC
import Database.HDBC.PostgreSQL

[yesh|
    -- name:insertUser :: (Integer)
    -- :name :: String
    INSERT INTO users (name) VALUES (:name) RETURNING id;
    -- name:deleteUser :: Integer
    -- :id :: Integer
    DELETE FROM users WHERE id = :id;
    -- name:getUser :: (Integer, String)
    -- :id :: Integer
    SELECT id, name FROM users WHERE id = :id;
    |]

dsn :: String
dsn = "host=localhost dbname=scrap user=scrap password=scrap"

main :: IO ()
main = do
    withPostgreSQL dsn . flip withTransaction $ \conn -> do
        uid:_ <- insertUser conn "niels"
        getUser conn uid >>= print
        getUser conn 1 >>= print
        deleteUser conn uid
        return ()
