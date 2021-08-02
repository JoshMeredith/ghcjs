{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Options.Applicative
import GHCJS.Linker


main :: IO ()
main = do
  (settings, files) <- execParser (info (linkerSettings <**> helper) mempty)
  link settings "link-output.js" files
