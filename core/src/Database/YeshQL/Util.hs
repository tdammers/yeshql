{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE CPP #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleInstances #-}
module Database.YeshQL.Util
where

import Prelude hiding (fail)
import Database.YeshQL.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote
#if MIN_VERSION_template_haskell(2,7,0)
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile))
#endif
import Data.Char (toLower, toUpper, isAlpha, isAlphaNum, chr, ord)
import System.FilePath (takeBaseName)
import Control.Monad.Fail

queryName :: String -> String -> Name
queryName prefix = mkName . queryIdentifier prefix

queryIdentifier :: String -> String -> String
queryIdentifier "" basename =
    lcfirst . makeValidIdentifier . takeBaseName $ basename
queryIdentifier prefix basename =
    (prefix ++) . ucfirst . makeValidIdentifier . takeBaseName $ basename

ucfirst :: String -> String
ucfirst "" = ""
ucfirst (x:xs) = toUpper x:xs

lcfirst :: String -> String
lcfirst "" = ""
lcfirst (x:xs) = toLower x:xs

makeValidIdentifier :: String -> String
makeValidIdentifier =
    filter isAlphaNum .
    dropWhile (not . isAlpha)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

nthIdent :: Int -> String
nthIdent i
    | i < 26 = [chr (ord 'a' + i)]
    | otherwise = let (j, k) = divMod i 26
                    in nthIdent j ++ nthIdent k

nameQuery :: String -> ParsedQuery -> ParsedQuery
nameQuery qname pq
    | null (pqQueryName pq) = pq { pqQueryName = qname }
    | otherwise = pq

nameQueries :: String -> [ParsedQuery] -> [ParsedQuery]
nameQueries basename queries =
    zipWith nameQuery queryNames queries
    where
        queryNames = [ basename ++ "_" ++ show i | i <- [0..] ]

withParsedQuery :: (MonadFail m, MonadPerformIO m, Monad m)
                => (ParsedQuery -> m a) -> String -> m a
withParsedQuery = withParsed parseQuery

withParsedQueries :: (MonadFail m, MonadPerformIO m, Monad m)
                  => ([ParsedQuery] -> m a) -> String -> m a
withParsedQueries = withParsed parseQueries

withParsedQueryFile :: (MonadFail m, MonadPerformIO m, Monad m)
                    => (ParsedQuery -> m a) -> FilePath -> m a
withParsedQueryFile p fn =
    withParsedFile
        (parseQueryN fn)
        (p . nameQuery (queryIdentifier "" fn))
        fn

withParsedQueriesFile :: (MonadFail m, MonadPerformIO m, Monad m)
                      => ([ParsedQuery] -> m a) -> FilePath -> m a
withParsedQueriesFile p fn =
    withParsedFile
        (parseQueriesN fn)
        (p . nameQueries (queryIdentifier "" fn))
        fn

withParsed :: (MonadFail m, Monad m, Show e)
           => (s -> Either e a) -> (a -> m b) -> s -> m b
withParsed p a src = do
    let parseResult = p src
    arg <- case parseResult of
                Left e -> fail . show $ e
                Right x -> return x
    a arg

-- | Monad in which we can perform IO and tag dependencies. Mostly needed
-- because we cannot easily make a 'MonadIO' instance for 'Q', and also
-- because we want to avoid a dependency on mtl or transformers. For
-- convenience, we also pull 'addDependentFile' into this typeclass.
class MonadPerformIO m where
    performIO :: IO a -> m a
    addDependentFile :: FilePath -> m ()

instance MonadPerformIO IO where
    performIO = id
    -- in IO, don't try to track dependencies
    addDependentFile = const $ return ()

instance MonadPerformIO Q where
    performIO = runIO
#if MIN_VERSION_template_haskell(2,7,0)
    -- modern GHC: proper implementation
    addDependentFile = qAddDependentFile
#else
    -- ancient GHC: ignore dependency
    addDependentFile = const $ return ()
#endif

withParsedFile :: (MonadFail m, MonadPerformIO m, Monad m, Show e) => (String -> Either e a) -> (a -> m b) -> FilePath -> m b
withParsedFile p a filename =
    addDependentFile filename >>
    performIO (readFile filename) >>=
        withParsed p a
