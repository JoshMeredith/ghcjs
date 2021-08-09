module Main where

import Options.Applicative
import GHCJS.Linker


main :: IO ()
main = do
  (settings, command) <- execParser (info (linkerSettings <**> helper) mempty)
  link settings command
