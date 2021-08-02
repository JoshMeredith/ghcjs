{-# LANGUAGE
  ApplicativeDo,
  OverloadedStrings
#-}

module GHCJS.Linker (
  module GHCJS.Linker.Compactor,
  module GHCJS.Linker.Monad,

  linkerSettings,
  link,
) where

-- Utilies
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Parallel.Strategies
import           Options.Applicative
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)
import qualified Data.ByteString.Lazy         as BL
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TLE

-- GHCJS
import Gen2.Object
import Gen2.Printer

-- GHCJS-LD
import GHCJS.Linker.Compactor
import GHCJS.Linker.Monad


linkerSettings :: Parser (LinkerSettings, FilePath, [FilePath])
linkerSettings = do
  debug  <- switch (long "debug")
  prof   <- switch (long "prof")
  dedup  <- not <$> switch (long "no-dedupe")
  files  <- some (argument str (metavar "file..."))
  output <- strOption
              ( long "output"
             <> short 'o'
             <> value "link-output.js"
              )
  return $ (LinkerSettings debug prof dedup, output, files)

link :: LinkerSettings -> FilePath -> [FilePath] -> IO ()
link settings output inputs = do

  code <- forM inputs $ \input -> do
    collectCode =<< readObjectFile input

  let rtsDeps = S.fromList []

  let compactm = compact (map funSymbol $ S.toList rtsDeps) (map (\(s,_,ci,si) -> (s,ci,si)) code)
      (compacted, meta) = runLinker compactm settings
      pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      renderedExports = TLE.encodeUtf8 . TL.fromStrict . T.unlines . filter (not . T.null) $ map (\(_,rs,_,_) -> rs) code

  BL.writeFile output $ mconcat rendered <> renderedMeta <> renderedExports

  where
    collectCode l = let x = ( mconcat (map oiStat l)
                            , T.unlines (map oiRaw l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l)
                    in evaluate (rnf x) >> return x