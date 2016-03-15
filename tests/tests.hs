{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty
import qualified Database.YeshQL.SimulationTests as SimulationTests

main = defaultMain allTests
    where
        allTests = testGroup "All Tests"
            [ testGroup "Simulation Tests" SimulationTests.tests
            ]
