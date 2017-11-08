{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL.ParserTests
( tests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Database.YeshQL
import Database.YeshQL.Parser
import qualified Data.Map as Map

tests =
  [ testAllTheThings
  ]

testAllTheThings :: TestTree
testAllTheThings =
  testCase "all parser features" $
    assertEqual "" expected actual
    where
      actual = parseQuery query
      query = unlines
        [ "-- name:perfectlyNormal :: (Integer)"
        , "-- A completely normal query."
        , "-- One more free-form comment."
        , "-- :username :: String"
        , "SELECT id FROM users WHERE username = :username"
        ]
      expected =
        Right ParsedQuery
          { pqQueryName = "perfectlyNormal"
          , pqQueryString = "SELECT id FROM users WHERE username = ?\n"
          , pqParamsRaw = [ExtractedParam "username" [] AutoType]
          , pqParamNames = ["username"]
          , pqParamTypes = Map.fromList [("username", PlainType "String")]
          , pqReturnType = ReturnTuple One [PlainType "Integer"]
          , pqDocComment = "A completely normal query.\nOne more free-form comment.\n"
          , pqDDL = False
          }
    
