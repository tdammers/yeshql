{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL.PostgreSQL.Tests
( tests
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.YeshQL.PostgreSQL
import System.IO
import Data.Char
import Data.List (dropWhile, dropWhileEnd)

data User = User
  { userID :: Int
  , userName :: String
  } deriving (Show, Eq)

instance FromRow User where
  fromRow = User <$> field <*> field

data Person = Person
  { personName :: String
  , personEmail :: String
  } deriving (Show, Eq)

data UserData = UserData
  { user :: User
  , person :: Person
  } deriving (Show, Eq)

instance FromRow UserData where
  fromRow =
    UserData
    <$> (User <$> field <*> field)
    <*> (Person <$> field <*> field)

tests conn =
  map
    ($ conn)
    [ testSimpleSelect
    , testSimpleSelectStr
    , testParametrizedSelect
    , testSingleInsert
    , testRecordReturn
    , testRecordReturnComplex
    , testRecordReturnExcessive
    , testTupleReturnMany
    , testRecordReturnMany
    , testRecordParams
    , testUpdateReturnRowCount
    , testMultiQuery
    , testQueryFromFile
    , testQueryFromFileAutoName
    , testDDL
    ]

testSimpleSelect :: Connection -> TestTree
testSimpleSelect conn = testCase "Simple SELECT" $ do
    results <- [yesh|
        -- name:getUserByName :: (String)
        SELECT username FROM users|] conn
    return ()

testEmptyReturn :: Connection -> TestTree
testEmptyReturn conn = testCase "Simple SELECT (empty return)" $ do
    [yesh|
        -- name:getUserByName :: ()
        SELECT username FROM users WHERE 0|] conn

testSimpleSelectStr :: Connection -> TestTree
testSimpleSelectStr conn = testCase "Simple SELECT (expr by string)" $ do
    results <- $(yesh $ unlines
        [ "-- name:getUserByName :: (String)"
        , "SELECT username FROM users"
        ]) conn
    return ()

testParametrizedSelect :: Connection -> TestTree
testParametrizedSelect conn = testCase "Parametrized SELECT" $ do
    actual <- [yesh|
        -- name:getUserByName :: (Integer, String)
        -- :username :: String
        SELECT id, username FROM users WHERE username = :username LIMIT 1|] conn "billy"
    let expected :: Maybe (Integer, String)
        expected = Just (1, "billy")
    assertEqual "" expected actual

testRecordReturn :: Connection -> TestTree
testRecordReturn conn = testCase "Return record from SELECT" $ do
    actual <- [yesh|
        -- name:getUserByName :: User
        -- :username :: String
        SELECT id, username FROM users WHERE username = :username LIMIT 1|]
        conn
        "billy"
    let expected :: Maybe User
        expected = Just $ User 1 "billy"
    assertEqual "" expected actual

testRecordReturnComplex :: Connection -> TestTree
testRecordReturnComplex conn = testCase "Return record from SELECT" $ do
    actual <- [yesh|
        -- name:getUserByName :: UserData
        -- :username :: String
        SELECT id, username, person_name, email FROM users WHERE username = :username and id = 1|]
        conn
        "billy"
    let expected :: Maybe UserData
        expected = Just $ UserData (User 1 "billy") (Person "Billy" "billy@example.org")
    assertEqual "" expected actual

testRecordReturnExcessive :: Connection -> TestTree
testRecordReturnExcessive conn = testCase "Return record from SELECT (extra values ignored)" $ do
    actual <- [yesh|
        -- name:getUserByName :: User
        -- :username :: String
        SELECT id, username FROM users WHERE username = :username LIMIT 1|]
        conn
        "billy"
    let expected :: Maybe User
        expected = Just $ User 1 "billy"
    assertEqual "" expected actual

testTupleReturnMany :: Connection -> TestTree
testTupleReturnMany conn = testCase "Return a list of single-element tuples" $ do
  actual <- [yesh|
    -- name:getUsers :: [(String)]
    select username from Users|] conn
  let expected = ["billy", "billy"]
  assertEqual "" expected actual

testRecordReturnMany :: Connection -> TestTree
testRecordReturnMany conn = testCase "Return a list of records from SELECT" $ do
  actual <- [yesh|
    -- name:getUsers :: [User]
    select id, username from Users|] conn
  let expected = [User 1 "billy", User 2 "billy"]
  assertEqual "" expected actual

testRecordParams :: Connection -> TestTree
testRecordParams conn = testCase "Pass records as params" $ do
    actual <- [yesh|
        -- name:getUserByName :: User
        -- :user :: User
        SELECT id, username FROM users WHERE username = :user.userName.reverse LIMIT 1|]
        conn
        (User 10 "yllib")
    let expected :: Maybe User
        expected = Just $ User 1 "billy"
    assertEqual "" expected actual

testSingleInsert :: Connection -> TestTree
testSingleInsert conn = testCase "Single INSERT" $ do
    actual <- [yesh|
        -- name:createUser :: (Integer)
        -- :username :: String
        INSERT INTO users (username) VALUES (:username) RETURNING id|] conn "billy"
    let expected :: Maybe Integer
        expected = Just 2
    assertEqual "" expected actual

testUpdateReturnRowCount :: Connection -> TestTree
testUpdateReturnRowCount conn = testCase "UPDATE, get row count" $ do
    actual <- [yesh|
        -- name:renameUser :: rowcount Integer
        -- :oldName :: String
        -- :newName :: String
        UPDATE users SET username = :oldName WHERE username = :newName AND username <> :oldName|]
            conn "tony" "billy"
    let expected :: Integer
        expected = 2
    assertEqual "" expected actual

[yesh|
    -- name:findUser :: (Int)
    -- :username :: String
    SELECT id FROM users WHERE username = :username
    ;;;
    -- name:setUserName :: rowcount Integer
    -- :userID :: Int
    -- :username :: String
    UPDATE users SET username = :username WHERE id = :userID
|]

testMultiQuery :: Connection -> TestTree
testMultiQuery conn = testCase "Define multiple queries in one QQ inside a where" $ do
    userID <- maybe (fail "User Not Found") return =<< findUser conn "tony"
    rowCount <- setUserName conn userID "billy"
    assertEqual "" rowCount 1

testQueryFromFile :: Connection -> TestTree
testQueryFromFile conn = testCase "Query loaded from fixture file" $ do
    [(userID, username)] <- [yesh1File|tests/fixtures/getUserNamed.sql|] conn 1
    assertEqual "" userID 1
    assertEqual "" username "billy"

[yesh1File|tests/fixtures/getUser.sql|]

testQueryFromFileAutoName :: Connection -> TestTree
testQueryFromFileAutoName conn = testCase "Query loaded from fixture file, deriving query name from filename" $ do
    [(userID, username)] <- getUser conn 1
    assertEqual "" userID 1
    assertEqual "" username "billy"

[yeshFile|tests/fixtures/multiQueries.sql|]

testManyQueriesFromFileAutoName :: Connection -> TestTree
testManyQueriesFromFileAutoName conn = testCase "Many queries from fixture file, auto-naming some" $ do
    count0 <- multiQueries_0 conn
    count1 <- multiQueriesNamed conn
    count2 <- multiQueries_2 conn
    assertEqual "" count0 1
    assertEqual "" count1 2
    assertEqual "" count2 3

testDDL :: Connection -> TestTree
testDDL conn = testCase "typical DDL statement sequence" $ do
    [yesh1|
        -- @ddl
        CREATE TABLE users_tmp
            ( id INTEGER
            , username TEXT
            );
        CREATE TABLE pages
            ( id INTEGER
            , owner_id INTEGER
            , title TEXT
            , body TEXT
            , FOREIGN KEY (owner_id) REFERENCES users (id)
            );
    |] conn
