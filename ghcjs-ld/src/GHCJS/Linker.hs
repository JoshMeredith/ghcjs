{-# LANGUAGE
  ApplicativeDo,
  FlexibleContexts,
  NamedFieldPuns,
  ScopedTypeVariables,
  OverloadedStrings,
  TupleSections
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
import           Data.Array                   (Array)
import qualified Data.Array                   as A
import qualified Data.ByteString.Lazy         as BL
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IS
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.MultiMap                (MultiMap)
import qualified Data.MultiMap                as MM
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TLE
import           Data.Time.Clock
import           Data.Traversable
import           System.Directory
import           System.Environment
import           System.FilePath

-- GHC
import Module

-- GHCJS
import Compiler.JMacro.Base                   (JStat)
import Gen2.Archive                           as Archive
import Gen2.ClosureInfo                       (ClosureInfo, StaticInfo)
import Gen2.Object                            as Object
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
   , roots       :: [Text]
   , runMain     :: Maybe FilePath
   }

verbosityOpt :: Parser Verbosity
verbosityOpt = (numerical <$> n) <|> (switch (short 'v') *> pure Verbosity1) <|> quiet
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
  verbose <- verbosityOpt
  debug   <- switch (long "debug")
  prof    <- switch (long "prof")
  dedup   <- not <$> switch (long "no-dedupe")
  paths   <- many (option auto (short 'L' <> long "library" <> metavar "libname"))
  libs    <- many (option auto (short 'l' <> long "library-path" <> metavar "searchdir"))
  relocatable <- switch (short 'r' <> long "relocatable")
  inputs  <- some (argument str (metavar "file..."))
  roots   <- many (option auto (long "root" <> metavar "link-tree root"))
  runMain <- optional $ strOption (long "runMain")
  return (LinkerSettings debug prof dedup verbose [], LinkCommand{output, inputs, paths, libs, relocatable, roots, runMain})

-- Setup and run the linking step - logging to console in verbose mode and displaying errors.
link :: (LinkerSettings) -> LinkCommand -> IO ()
link set@LinkerSettings{lsVerbosity = verbose} comm = do
  print ("link", verbose)
  linked <- runLinker (link' comm) set
  case linked of
    Right () -> return ()
    Left MissingLibraries -> error "Exiting due to missing libraries"
    Left DependencyError  -> error "Exiting due to dependency inconsistencies"

-- Perform the linking step based on a `LinkCommand`.
link' :: (Linker m, MonadIO m, MonadError LinkError m) => LinkCommand -> m ()
link' command@LinkCommand{output, inputs, paths, libs, relocatable, roots, runMain} = phase "Linker" $ do
  let rootMods = [] :: [Text]
  whenM (verbosity Verbosity1) $ when relocatable $
    logWarning "Warning: ignoring `-r`/`--relocatable` flag"
  libPaths <- getLibraries paths libs

  let ( oInputs, inputs' ) = partition ("o"  `isSuffixOf`) inputs
      ( aInputs, inputs'') = partition ("a"  `isSuffixOf`) inputs'
      (jsInputs, remain  ) = partition ("js" `isSuffixOf`) inputs''

  when (not $ null remain) $ do
    logError ("unknown input files " ++ show remain)
    exitLinker DependencyError

  if relocatable
    then do
      when (not $ null jsInputs) $ do
        logError ("cannot link a `.js` file with `-r`/`--relocatable`")
        exitLinker DependencyError
        linkRelocatable output aInputs oInputs jsInputs
    else linkFiles output runMain (map parseRoot roots) oInputs (aInputs ++ libPaths) jsInputs

linkRelocatable :: (Linker m, MonadError LinkError m, MonadIO m) => FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> m ()
linkRelocatable output aInputs oInputs jsInputs = do
  oModules <- liftIO $ forM oInputs $ \o -> do
                dep <- readDepsFile o
                dat <- readObjectFile o
                return (dep, dat)
  metas    <- liftIO $ forM aInputs (readMeta >=> (return . metaCppOptions))
  aModules <- fmap concat . liftIO $ forM aInputs $ \a -> do
                ss <- readAllSources a
                forM ss $ \(src, raw) ->
                  return (readDeps src raw, Object.readObject src raw)
  jsModules <- forM jsInputs $ \js -> (JsSource js,) <$> BS.readFile js


  liftIO . BL.writeFile output . buildArchive (Meta $ concat metas) $ map go (oModules ++ aModules) ++ jsModules

  where
    go (dep, dat) = (Object $ depsModule dep, object dep dat)  

-- Perform the linking step with an output location and a set of input files.
linkFiles :: (Linker m, MonadError LinkError m, MonadIO m) => FilePath -> Maybe FilePath -> [Fun] -> [FilePath] -> [FilePath] -> [FilePath] -> m ()
linkFiles output runMainFile roots oInputs aInputs jsInputs = phase "linkFiles" $ do
  
  oDeps <- liftIO $ forM oInputs $ \f -> (False, f,) <$> readDepsFile f
  aDeps <- fmap concat $ forM aInputs $ \l -> liftIO $
             withAllObjects l (\m h n -> (True, l,) <$> hReadDeps (moduleNameString m) h)

  let oaDeps = oDeps ++ aDeps
      deps :: Map (Package, Text) ((Bool, FilePath), IntSet)
      deps = foldl' collectDeps M.empty oaDeps
      blockDeps :: Map (Package, Text, Int) BlockDeps
      blockDeps = foldl' collectBlocks M.empty oaDeps
      funs :: Map Fun Int
      funs = foldl' collectFuns M.empty oaDeps

      roots' | not $ null roots = map (lookupFuns funs) roots
             | otherwise        = concatMap (\((p, m), (_, ixs)) -> map (p, m,) (IS.toList ixs)) (M.toList deps)

      toLink = traceDeps (lookupDeps deps blockDeps)
                         (lookupFuns funs)
                         roots'
                         S.empty

      linkFiles = MM.fromList $ map (\(p, m, i) -> ((p, m), i)) (S.toList toLink)

  code <- forM (MM.assocs linkFiles) $ \((p, m), ixs) -> do
    case M.lookup (p, m) deps of
      Nothing -> exitLinker DependencyError
      Just ((False, object ), _) -> liftIO $ readObjectFileKeys (\i _ -> i `elem` ixs) object
      Just ((True , archive), _) -> liftIO $ readObjectKeys archive (\i _ -> i `elem` ixs) <$>
                                               Archive.readObject (mkModuleName $ T.unpack m) archive

  linked <- linkObjects code

  javascript <- liftIO $ mapM BL.readFile jsInputs

  runMain <- case runMainFile of
               Just r  -> (:[]) <$> liftIO (BL.readFile r)
               Nothing -> return []

  liftIO $ BL.writeFile output (mconcat $ javascript ++ [linked] ++ runMain)

  where
    collectFuns   fs (a, f, d) = M.union fs (depsHaskellExported d)
    collectDeps   ds (a, f, d) = M.insert (depsPackage d, depsModule d) ((a, f), depsRequired d) ds
    collectBlocks bs (a, f, d) = foldl' go bs (A.assocs $ depsBlocks d)
      where go bs' (ix, bd) = M.insert (depsPackage d, depsModule d, ix) bd bs'

    lookupDeps deps blockDeps (p, m, i) =
      case (M.lookup (p, m) deps, M.lookup (p, m, i) blockDeps) of
        (Just (_, ds), Just (BlockDeps bds fds)) -> (IS.union ds (IS.fromList bds), fds)
        _ -> (IS.empty, [])

    lookupFuns funs f@(Fun p m n) =
      case M.lookup f funs of
        Just i  -> (p, m, i)
        Nothing -> error $ "Missing function" ++ show f

parseRoot :: Text -> Fun
parseRoot symbol = Fun (Package p) m f
  where
    p = T.takeWhile (/= '.') symbol
    m = T.reverse . T.drop (T.length f + 1) . T.reverse $ T.drop (T.length p + 1) symbol
    f = T.reverse . T.takeWhile (/= '.') $ T.reverse symbol

traceDeps :: ((Package, Text, Int) -> (IntSet, [Fun]))
          -> (Fun -> (Package, Text, Int))
          -> [(Package, Text, Int)]
          -> Set (Package, Text, Int)
          -> Set (Package, Text, Int)
traceDeps _ _ [] links = links
traceDeps deps locate roots links = traceDeps deps locate newIxs links'
  where
    links' = S.union links (S.fromList newIxs)
    (local, funs) = unzip $ map go roots
    go (p, m, i) = let (ls, funs) = deps (p, m, i) in (map (p, m,) $ IS.toList ls, funs)
    ixs = concat local ++ map locate (concat funs)
    newIxs = filter (`S.member` links) ixs

-- Provide a rendered output javascript given a set of input objects.
linkObjects :: (Linker m, MonadIO m)
            => [[ObjUnit]]
            -> m BL.ByteString
linkObjects code = phase "linkObjects" $ do
  let rtsDeps = S.fromList []

  c <- liftIO $ mapM collectCode code
      
  (compacted, meta) <- compact (map funSymbol $ S.toList rtsDeps) (map (\(s,_,ci,si) -> (s,ci,si)) c)

  let pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      renderedExports = TLE.encodeUtf8 . TL.fromStrict . T.unlines . filter (not . T.null) $ map (\(_,rs,_,_) -> rs) c

  return $ mconcat rendered <> renderedMeta <> renderedExports

  where
    collectCode l = let x = ( mconcat (map oiStat l)
                            , T.unlines (map oiRaw l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l)
                    in evaluate (rnf x) >> return x

-- Given a list of search paths and a list of library names, provide a list to the locations
-- of the object files for those libraries.
getLibraries :: (Linker m, MonadIO m, MonadError LinkError m) => [FilePath] -> [String] -> m [FilePath]
getLibraries paths libs = phase "getLibraries" $ do
  candidates <- join <$> mapM getCandidates paths
  mpaths <- forM libs $ \l -> do
    libPath <- matchLib candidates l <$> linkProf
    whenM (verbosity Verbosity3) . logMessage . concat $ case libPath of
      Nothing -> ["file for library \"", l, "\" not found"]
      Just p  -> ["file for library \"", l, "\":", p]
    return (l, libPath)
  forM mpaths $ \(l, libPath) -> case libPath of
      Nothing -> throwError MissingLibraries
      Just p  -> return p

getCandidates :: (Linker m, MonadIO m) => FilePath -> m [FilePath]
getCandidates path = phase "getCandidates" $ do
  logMessage $ concat ["Libraries found at \"", path, "\":"]
  files <- map (path </>) <$> liftIO (listDirectory path)
  mapM_ logMessage files
  return $ filter (\fp -> "js_a" `isExtensionOf` fp) files

matchLib :: [FilePath] -> String -> Bool -> Maybe FilePath
matchLib files lib prof = find matching files
  where
    matching f = lib `isInfixOf` takeBaseName f
              && ("_p" `isSuffixOf` takeBaseName f == prof)