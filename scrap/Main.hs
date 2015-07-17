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
    -- name:getUserEx :: (Integer, String)
    -- :id :: Integer
    -- :filename :: String
    -- :unused :: Double
    SELECT id, name FROM users WHERE name = :filename OR id = :id;
    |]

dsn :: String
dsn = "host=localhost dbname=scrap user=scrap password=scrap"

main :: IO ()
main = do
    withPostgreSQL dsn . flip withTransaction $ \conn -> do
        uid:_ <- insertUser "niels" conn
        getUserEx uid "niels" 0.5 conn >>= print
        getUser 1 conn >>= print
        deleteUser uid conn
        return ()
