{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty
import qualified Database.YeshQL.ParserTests as ParserTests

main = defaultMain allTests
    where
        allTests = testGroup "All Tests"
            [ testGroup "Parser Tests" ParserTests.tests
            ]
