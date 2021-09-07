module Main where

import Options.Applicative
import GHCJS.Linker


main :: IO ()
main = uncurry link =<< execParser (info (linkerSettings <**> helper) mempty)
