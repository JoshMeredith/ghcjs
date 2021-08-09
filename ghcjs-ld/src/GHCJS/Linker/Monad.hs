{-# LANGUAGE
  ConstraintKinds,
  FlexibleContexts,
  LambdaCase,
  OverloadedStrings
#-}

module GHCJS.Linker.Monad where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.Functor.Identity
import Data.Text
import Data.Text.IO
import GHC.IO.Handle.FD (stdout, stderr)

import Gen2.Base


data Verbosity = Quiet | Verbosity1 | Verbosity2 | Verbosity3 deriving (Eq, Ord)

data LinkerSettings
   = LinkerSettings
   { lsDebug  :: Bool
   , lsProf   :: Bool
   , lsDedupe :: Bool
   , lsVerbosity :: Verbosity
   }

type Link = LinkT Identity
type LinkT m = RWST LinkerSettings [Either Text Text] CompactorState m

type Linker m = ( MonadReader LinkerSettings m
                , MonadState CompactorState m
                , MonadWriter [Either Text Text] m
                )


-- Run a `Link` monad, ignoring the log messages.
runLinker :: Link a -> LinkerSettings -> a
runLinker m = runIdentity . runLinkerT m

-- Run a `LinkT` monad transformer, ignoring the log messages.
runLinkerT :: Monad m => LinkT m a -> LinkerSettings -> m a
runLinkerT m s = fst <$> evalRWST m s emptyCompactorState

-- Run a `LinkT` monad transformer, then print it's log messages to the relevant stdout/stderr.
consoleLinker :: MonadIO m => LinkT m a -> LinkerSettings -> m a
consoleLinker m s = do
  (a, messages) <- evalRWST (linkLog "testlog" >> linkWarn "testWarn" >> m) s emptyCompactorState
  liftIO $ forM_ messages $ \case
    Left  warn -> hPutStrLn stderr warn
    Right log  -> hPutStrLn stdout log
  return a

linkDebug :: MonadReader LinkerSettings m => m Bool
linkDebug = lsDebug <$> ask

linkProf  :: MonadReader LinkerSettings m => m Bool
linkProf = lsProf <$> ask

linkDedupe :: MonadReader LinkerSettings m => m Bool
linkDedupe = lsDedupe <$> ask

-- Log a warning - to be sent to stderr.
linkWarn :: MonadWriter [Either Text a] m => Text -> m ()
linkWarn warn = tell [Left warn]

-- Log a message - to be sent to stdout.
linkLog :: MonadWriter [Either a Text] m => Text -> m ()
linkLog log = tell [Right log]

-- Is the setting verbosity at least the argument verbosity?
-- E.g. `whenM (verbosity Verbosity1) $ linkLog "..."`
verbosity :: MonadReader LinkerSettings m => Verbosity -> m Bool
verbosity v = (>= v) <$> (lsVerbosity <$> ask)
