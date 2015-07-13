{-#LANGUAGE TemplateHaskell #-}
module Main where

import Database.YeshQL

$(mkQuery "testQ" "SELECT * FROM test WHERE id = :id")

main = do
    putStrLn "Hello"
