{-# LANGUAGE
  ApplicativeDo,
  FlexibleContexts,
  NamedFieldPuns,
  ScopedTypeVariables,
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
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Options.Applicative
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)
import qualified Data.ByteString.Lazy         as BL
import           Data.List
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TLE
import           System.Directory
import           System.FilePath

-- GHCJS
import Compiler.JMacro.Base                   (JStat)
import Gen2.ClosureInfo                       (ClosureInfo, StaticInfo)
import Gen2.Object
import Gen2.Printer

-- GHCJS-LD
import GHCJS.Linker.Compactor
import GHCJS.Linker.Monad


data LinkCommand
   = LinkCommand
   { output      :: FilePath
   , inputs      :: [FilePath]
   , paths       :: [FilePath]
   , libs        :: [String]
   , relocatable :: Bool
   }

data LinkError
   = LibNotFound String

verbosityOpt :: Parser Verbosity
verbosityOpt = (numerical <$> n) <|> quiet
  where
    n     = option auto (short 'v')
    quiet = pure Quiet
    numerical 1 = Verbosity1
    numerical 2 = Verbosity2
    numerical 3 = Verbosity3

linkerSettings :: Parser (LinkerSettings, LinkCommand)
linkerSettings = do
  output <- strOption
              ( long "output"
             <> short 'o'
             <> value "link-output.js"
              )
  debug  <- switch (long "debug")
  prof   <- switch (long "prof")
  dedup  <- not <$> switch (long "no-dedupe")
  verbose <- verbosityOpt
  paths   <- many (option auto (short 'L' <> long "library" <> metavar "libname"))
  libs    <- many (option auto (short 'l' <> long "library-path" <> metavar "searchdir"))
  relocatable <- switch (short 'r' <> long "relocatable")
  inputs <- some (argument str (metavar "file..."))
  return (LinkerSettings debug prof dedup verbose, LinkCommand{output, inputs, paths, libs, relocatable})

-- Setup and run the linking step - logging to console in verbose mode and displaying errors.
link :: LinkerSettings -> LinkCommand -> IO ()
link set@LinkerSettings{lsVerbosity = verbose} comm = do
  linked <- runExceptT $
        if verbose >= Verbosity1
          then consoleLinker (link' comm) set
          else runLinkerT    (link' comm) set
  case linked of
    Right () -> return ()
    Left (LibNotFound l) -> error $ "Cannot find library: \"" ++ l ++ "\""

-- Perform the linking step based on a `LinkCommand`.
link' :: (Linker m, MonadIO m, MonadError LinkError m) => LinkCommand -> m ()
link' command@LinkCommand{output, inputs, paths, libs, relocatable} = do
  whenM (verbosity Verbosity1) $ when relocatable $
    linkWarn "Warning: ignoring `-r`/`--relocatable` flag"
  libPaths <- getLibraries paths libs
  linkFiles output (inputs ++ libPaths)

-- Perform the linking step with an output location and a set of input files.
linkFiles :: (Linker m, MonadIO m) => FilePath -> [FilePath] -> m ()
linkFiles output inputs = do
  code <- liftIO $ forM inputs $ collectCode <=< readObjectFile

  linked <- linkObjects code
  liftIO $ BL.writeFile output linked

  where
    collectCode l = let x = ( mconcat (map oiStat l)
                            , T.unlines (map oiRaw l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l)
                    in evaluate (rnf x) >> return x

-- Provide a rendered output javascript given a set of input objects.
linkObjects :: Linker m => [(JStat, Text, [ClosureInfo], [StaticInfo])] -> m BL.ByteString
linkObjects code = do
  let rtsDeps = S.fromList []
      
  (compacted, meta) <- compact (map funSymbol $ S.toList rtsDeps) (map (\(s,_,ci,si) -> (s,ci,si)) code)

  let pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      renderedExports = TLE.encodeUtf8 . TL.fromStrict . T.unlines . filter (not . T.null) $ map (\(_,rs,_,_) -> rs) code

  return $ mconcat rendered <> renderedMeta <> renderedExports

-- Given a list of search paths and a list of library names, provide a list to the locations
-- of the object files for those libraries.
getLibraries :: (Linker m, MonadIO m, MonadError LinkError m) => [FilePath] -> [String] -> m [FilePath]
getLibraries paths libs = do
  candidates <- join <$> mapM getCandidates paths
  mpaths <- forM libs $ \l -> do
    let libPath = matchLib candidates l
    whenM (verbosity Verbosity3) . linkLog . T.concat $ case libPath of
      Nothing -> ["File for library \"", T.pack l, "\" not found"]
      Just p  -> ["File for library \"", T.pack l, "\":", T.pack p]
    return (l, libPath)
  forM mpaths $ \(l, libPath) -> case libPath of
      Nothing -> throwError $ LibNotFound l
      Just p  -> return p

getCandidates :: (Linker m, MonadIO m) => FilePath -> m [FilePath]
getCandidates path = do
  linkLog $ T.concat ["Libraries found at \"", T.pack path, "\":"]
  files <- liftIO $ listDirectory path
  mapM_ (linkLog . T.pack) files
  return $ filter (\fp -> "js_o" `isExtensionOf` fp || "js_a" `isExtensionOf` fp) files

matchLib :: [FilePath] -> String -> Maybe FilePath
matchLib files lib = find matching files
  where
    matching f = takeBaseName f `elem` [lib, "lib-" ++ lib]