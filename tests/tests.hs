module Main where

import Test.Tasty

main = defaultMain allTests
    where
        allTests = testGroup "All Tests" []
