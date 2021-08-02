module Main where

import Options.Applicative
import GHCJS.Linker


main :: IO ()
main = do
  (settings, output, files) <- execParser (info (linkerSettings <**> helper) mempty)
  link settings output files
