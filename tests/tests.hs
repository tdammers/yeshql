{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty
import qualified Database.YeshQL.SimulationTests as SimulationTests
import qualified Database.YeshQL.ParserTests as ParserTests

main = defaultMain allTests
    where
        allTests = testGroup "All Tests"
            [ testGroup "Simulation Tests" SimulationTests.tests
            , testGroup "Parser Tests" ParserTests.tests
            ]
