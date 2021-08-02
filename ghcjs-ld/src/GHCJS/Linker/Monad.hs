{-# LANGUAGE
  ConstraintKinds,
  FlexibleContexts
#-}

module GHCJS.Linker.Monad where

import Control.Monad.State.Strict
import Control.Monad.Reader

import Gen2.Base


data LinkerSettings
   = LinkerSettings
   { lsDebug  :: Bool
   , lsProf   :: Bool
   , lsDedupe :: Bool
   }

type Link = ReaderT LinkerSettings (State CompactorState)

type Linker m = ( MonadReader LinkerSettings m
                , MonadState CompactorState m
                )


runLinker :: Link a -> LinkerSettings -> a
runLinker m s = evalState (runReaderT m s) emptyCompactorState

linkDebug :: Linker m => m Bool
linkDebug = lsDebug <$> ask

linkProf  :: Linker m => m Bool
linkProf = lsProf <$> ask

linkDedupe :: Linker m => m Bool
linkDedupe = lsDedupe <$> ask