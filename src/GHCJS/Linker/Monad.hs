{-# LANGUAGE
  ConstraintKinds,
  FlexibleContexts,
  GeneralizedNewtypeDeriving,
  LambdaCase,
  OverloadedStrings
#-}

module GHCJS.Linker.Monad where

import Prelude

import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Data.Functor.Identity
import Data.String
import Data.Text
import Data.Time.Clock
import GHC.IO.Handle.FD (stdout, stderr)
import System.IO

import Gen2.Base


data Verbosity = Quiet | Verbosity1 | Verbosity2 | Verbosity3 deriving (Eq, Ord, Show)

data LinkError
   = MissingLibraries
   | DependencyError
   | WriteError

data LinkerSettings
   = LinkerSettings
   { lsDebug     :: Bool
   , lsProf      :: Bool
   , lsDedupe    :: Bool
   , lsVerbosity :: Verbosity
   , lsPhase     :: [String]
   , logPath     :: Maybe FilePath
   }

data LinkLog = LogMessage String | LogWarning String | LogError String

newtype Link a = Link { getLinker :: ReaderT LinkerSettings (StateT CompactorState (ExceptT LinkError IO)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError LinkError
           , MonadReader LinkerSettings
           , MonadState CompactorState
           )

type Linker m = ( MonadReader LinkerSettings m
                , MonadState CompactorState m
                , ConsoleLog m
                )

class ConsoleLog m where
  logMessage :: String -> m ()
  logWarning :: String -> m ()
  logError   :: String -> m ()
  exitLinker :: LinkError -> m a
  phase      :: String -> m a -> m a

instance ConsoleLog Link where
  logMessage = linkLog stdout Verbosity1 . ("[info]    " ++)
  logWarning = linkLog stderr Verbosity1 . ("[warning] " ++)
  logError   = linkLog stderr Quiet      . ("[error]   " ++)
  exitLinker e = do
    p <- getPhase
    linkLog stderr Quiet ("[exit]    " ++ show p)
    throwError e
  phase name m = local loc (ifM (verbosity Verbosity3) m' m)
    where
      loc r = r{lsPhase = name : lsPhase r}

      m' = do
        p       <- getPhase
        liftIO $ putStrLn (">>> " ++ show p)
        start   <- liftIO getCurrentTime
        x       <- m
        elapsed <- (`diffUTCTime` start) <$> liftIO getCurrentTime
        liftIO $ putStrLn ("<<< " ++ show p ++ " complete in " ++ show elapsed)
        return x

linkLog :: (MonadIO m, MonadReader LinkerSettings m) => Handle -> Verbosity -> String -> m ()
linkLog h v l = do
  whenM (verbosity Verbosity1) . liftIO $ hPutStrLn h l
  logFile <- logPath <$> ask
  case logFile of
    Nothing -> return ()
    Just f  -> liftIO $ appendFile f (l ++ "\n")


runLinker :: Link a -> LinkerSettings -> IO (Either LinkError a)
runLinker m s = runExceptT $ fmap fst (runStateT (runReaderT (getLinker m) s) emptyCompactorState)

linkDebug :: MonadReader LinkerSettings m => m Bool
linkDebug = lsDebug <$> ask

linkProf :: MonadReader LinkerSettings m => m Bool
linkProf = lsProf <$> ask

linkDedupe :: MonadReader LinkerSettings m => m Bool
linkDedupe = lsDedupe <$> ask

verbosity :: MonadReader LinkerSettings m => Verbosity -> m Bool
verbosity v = (>= v) <$> (lsVerbosity <$> ask)

getPhase :: MonadReader LinkerSettings m => m [String]
getPhase = (Prelude.reverse . lsPhase) <$> ask
