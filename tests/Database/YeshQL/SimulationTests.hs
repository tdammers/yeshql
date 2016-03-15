{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE RankNTypes #-}
module Database.YeshQL.SimulationTests
( tests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Database.HDBC
import Database.HDBC.Mock
import Database.YeshQL
import System.IO
import Data.Char
import Data.List (dropWhile, dropWhileEnd)

tests =
    [ testSimpleSelect
    , testParametrizedSelect
    , testSingleInsert
    , testUpdateReturnRowCount
    ]

testSimpleSelect :: TestTree
testSimpleSelect = testCase "Simple SELECT" $ chatTest chatScript $ \conn -> do
    results <- [yesh|
        -- name:getUserByName :: (String)
        SELECT username FROM users|] conn
    return ()
    where
        chatScript =
            [ ChatStep
                { chatQuery = sameThrough trim "SELECT username FROM users"
                , chatParams = []
                , chatResultSet = []
                , chatColumnNames = ["username"]
                , chatRowsAffected = 0
                }
            ]

testParametrizedSelect :: TestTree
testParametrizedSelect = testCase "Parametrized SELECT" $ chatTest chatScript $ \conn -> do
    actual <- [yesh|
        -- name:getUserByName :: (Integer, String)
        -- :username :: String
        SELECT id, username FROM users WHERE username = :username|] "billy" conn
    let expected :: [(Integer, String)]
        expected = [(1, "billy")]
    assertEqual "" expected actual
    where
        chatScript =
            [ ChatStep
                { chatQuery = sameThrough trim "SELECT id, username FROM users WHERE username = ?"
                , chatParams = [exactly (toSql "billy")]
                , chatResultSet = [[toSql (1 :: Int), toSql "billy"]]
                , chatColumnNames = ["username"]
                , chatRowsAffected = 0
                }
            ]

testSingleInsert :: TestTree
testSingleInsert = testCase "Single INSERT" $ chatTest chatScript $ \conn -> do
    actual <- [yesh|
        -- name:createUser :: (Integer)
        -- :username :: String
        INSERT INTO users (username) VALUES (:username) RETURNING id|] "billy" conn
    let expected :: [Integer]
        expected = [23]
    assertEqual "" expected actual
    where
        chatScript =
            [ ChatStep
                { chatQuery = sameThrough trim "INSERT INTO users (username) VALUES (?) RETURNING id"
                , chatParams = [exactly $ toSql "billy"]
                , chatResultSet = [[toSql (23 :: Int)]]
                , chatColumnNames = ["id"]
                , chatRowsAffected = 1
                }
            ]

testUpdateReturnRowCount :: TestTree
testUpdateReturnRowCount = testCase "UPDATE, get row count" $ chatTest chatScript $ \conn -> do
    actual <- [yesh|
        -- name:renameUser :: Integer
        -- :oldName :: String
        -- :newName :: String
        UPDATE users SET username = :oldName WHERE username = :newName AND username <> :oldName|]
            "tony" "billy" conn
    let expected :: Integer
        expected = 1
    assertEqual "" expected actual
    where
        chatScript =
            [ ChatStep
                { chatQuery = sameThrough trim
                    "UPDATE users SET username = ? WHERE username = ? AND username <> ?"
                , chatParams =
                    [ exactly $ toSql "tony"
                    , exactly $ toSql "billy"
                    , exactly $ toSql "tony"
                    ]
                , chatResultSet = []
                , chatColumnNames = []
                , chatRowsAffected = 1
                }
            ]
chatTest :: [ChatStep] -> (forall conn. IConnection conn => conn -> IO ()) -> Assertion
chatTest chatScript action =
    newChatConnection chatScript >>= action

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
