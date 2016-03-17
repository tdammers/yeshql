-- | Mock HDBC database connection for unit testing purposes.
module Database.HDBC.Mock
( 
-- * Mock Connections
  MockConnection (..)
, defMockConnection
-- * Logging interaction
, loggingConnection
-- * Chat Scripts
, newChatConnection
, ChatStep (..)
-- * Constraints
-- The Contraint API is a minilanguage for declaring and combining constraints
-- on arbitrary values.
, Constraint
-- | Construct a constraint from a raw predicate.
, constraint
-- | Check a constraint against a value.
, check
-- | Check a list of constraints against a list of values.
, checkAll
-- | A constraint that matches a value exactly (as per 'Eq' / '==')
, exactly
-- | A constraint that matches a value by a comparison function
, sameBy
-- | A constraint that matches a value exactly after applying some pre-processing
, sameThrough
-- | A constraint that matches any value
, anything
-- | The inclusive-OR operator on 'Constraint's.
, (<||>)
)
where

import Database.HDBC
            ( IConnection (..)
            , SqlValue (..)
            , SqlColDesc (..)
            )
import Database.HDBC.Statement
            ( Statement (..)
            )
import Control.Monad (forM_, when)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TChan
    ( TChan
    , newTChan
    , writeTChan
    , readTChan
    , tryReadTChan
    , newTChanIO
    )

data MockConnection =
    MockConnection
        { mockDisconnect :: IO ()
        , mockCommit :: IO ()
        , mockRollback :: IO ()
        , mockRun :: String -> [SqlValue] -> IO Integer
        , mockPrepare :: String -> IO Statement
        , mockClone :: IO MockConnection
        , mockTransactionSupport :: Bool
        , mockGetTables :: IO [String]
        , mockDescribeTable :: String -> IO [(String, SqlColDesc)]
        }

defMockConnection :: MockConnection
defMockConnection =
    MockConnection
        { mockDisconnect = return ()
        , mockCommit = return ()
        , mockRollback = return ()
        , mockRun = \q p -> fail "'run' method not implemented"
        , mockPrepare = \q -> fail "'prepare' method not implemented"
        , mockClone = fail "'clone' method not implemented"
        , mockTransactionSupport = False
        , mockGetTables = return []
        , mockDescribeTable = const $ return []
        }

instance IConnection MockConnection where
    disconnect = mockDisconnect
    commit = mockCommit
    rollback = mockRollback
    run = mockRun
    prepare = mockPrepare
    clone = mockClone
    hdbcDriverName = const "mock"
    hdbcClientVer = const "0.0"
    dbServerVer = const "0.0"
    dbTransactionSupport = mockTransactionSupport
    getTables = mockGetTables
    describeTable = mockDescribeTable

tee :: (a -> IO ()) -> IO a -> IO a
tee sideChannel action = do
    retval <- action
    sideChannel retval
    return retval

loggingStatement :: String -> (String -> IO ()) -> Statement -> Statement
loggingStatement query log stmt =
    stmt
        { execute = \params -> do
            log $ "execute " ++ show query ++ " with " ++ show params
            tee (log . show) $ execute stmt params
        , executeRaw = do
            log $ "executeRaw " ++ show query
            tee (log . show) $ executeRaw stmt
        , executeMany = \rows -> do
            log $ "executeMany " ++ show query ++ " with: "
            forM_ rows $ \params -> log $ "    " ++ show params
            tee (log . show) $ executeMany stmt rows
        , finish = do
            log $ "finish"
            finish stmt
        }

loggingConnection :: IConnection conn => (String -> IO ()) -> conn -> MockConnection
loggingConnection log conn =
    defMockConnection
        { mockDisconnect = do
            log "disconnect"
            disconnect conn
        , mockCommit = do
            log "commit"
            commit conn
        , mockRollback = do
            log "rollback"
            rollback conn
        , mockRun = \q p -> do
            log $ "run " ++ show q ++ " with " ++ show p
            tee (log . show) $ run conn q p
        , mockPrepare = \q -> do
            log $ "prepare " ++ show q
            loggingStatement q log <$> prepare conn q
        , mockClone = do
            log $ "clone"
            loggingConnection log <$> clone conn
        , mockTransactionSupport = dbTransactionSupport conn
        , mockGetTables = do
            log $ "getTables"
            tee (log . show) $ getTables conn
        , mockDescribeTable = \tbl -> do
            log $ "describeTable " ++ tbl
            tee (log . show) $ describeTable conn tbl
        }

tChanFromList :: [a] -> IO (TChan a)
tChanFromList xs = atomically $ do
    newTChan >>= fillTChanFromList xs

fillTChanFromList :: [a] -> (TChan a) -> STM (TChan a)
fillTChanFromList xs c = do
    forM_ xs $ writeTChan c
    return c

newtype Constraint a = Constraint { check :: a -> Bool }

instance Monoid (Constraint a) where
    mempty = anything
    mappend (Constraint a) (Constraint b) =
        Constraint (\x -> a x && b x)

(<||>) :: Constraint a -> Constraint a -> Constraint a
a <||> b = Constraint $ \x -> check a x || check b x

constraint :: (a -> Bool) -> Constraint a
constraint = Constraint

exactly :: Eq a => a -> Constraint a
exactly = sameBy (==)

sameThrough :: Eq a => (a -> a) -> a -> Constraint a
sameThrough pp lhs = constraint $ \rhs -> pp lhs == pp rhs

sameBy :: (a -> a -> Bool) -> a -> Constraint a
sameBy cmp val = constraint $ cmp val

anything :: Constraint a
anything = constraint (const True)

data ChatStep =
    ChatStep
        { chatQuery :: Constraint String
        , chatParams :: [Constraint SqlValue]
        , chatResultSet :: [[SqlValue]]
        , chatColumnNames :: [String]
        , chatRowsAffected :: Integer
        }

checkAll :: [Constraint a] -> [a] -> Bool
checkAll constraints values =
    all id $ zipWith check constraints values

execChatStep :: String -> [SqlValue] -> TChan [SqlValue] -> ChatStep -> IO Integer
execChatStep query params resultChan step = do
    when (not $ check (chatQuery step) query) $
        fail $
            "Chat script mismatch: unexpected query:\n\t" ++
            query
    when (not $ checkAll (chatParams step) params) $
        fail $
            "Parameter mismatch: got " ++ show params
    atomically $ fillTChanFromList (chatResultSet step) resultChan
    return $ chatRowsAffected step

newChatConnection :: [ChatStep] -> IO MockConnection
newChatConnection steps = do
    c <- tChanFromList steps
    mkChatConnection c
    where
        mkChatConnection c =
            return $ defMockConnection
                { mockRun = \q p -> do
                    resultVar <- newTChanIO
                    step <- atomically $ readTChan c
                    execChatStep q p resultVar step
                , mockPrepare = \q -> do
                    resultVar <- newTChanIO
                    let exec p = do
                            atomically (readTChan c) >>= execChatStep q p resultVar
                    return $
                        Statement
                            { execute = exec
                            , executeRaw = exec [] >> return ()
                            , executeMany = \ps -> mapM exec ps >> return ()
                            , finish = return ()
                            , fetchRow = atomically $ tryReadTChan resultVar
                            , getColumnNames = return []
                            , originalQuery = q
                            , describeResult = return []
                            }
                , mockClone = mkChatConnection c
                }
