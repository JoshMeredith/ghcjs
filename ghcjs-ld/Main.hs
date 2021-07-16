{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Compiler.JMacro.Base
import Gen2.Object
import Gen2.Base
import Compiler.Settings

import DynFlags
import Util

import           Control.Monad.State.Strict

import Data.Array
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Text as T
import Control.Monad
import Control.DeepSeq
import Control.Exception
import System.Environment
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Set as S
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Gen2.Printer (pretty)
import Text.PrettyPrint.Leijen.Text (displayT, renderPretty)
import Control.Parallel.Strategies
import Data.Maybe
import Gen2.ClosureInfo
import qualified Data.ByteString.Base64 as B64
import           Data.Function (on)
import Data.Char (chr)

import Compiler.JMacro.Combinators

import Control.Lens hiding ((#))
import Data.List
import Data.List.Split

import qualified Gen2.Utils as U
import Gen2.Utils (buildingDebug, buildingProf)

import qualified Gen2.Compactor as Compactor
import Gen2.Compactor (LinkedUnit, exprsE, exprsS, identsS, lookupRenamed, staticInfoArgs, dedupeBodies,
                       dedupe, renameStaticInfo, renameClosureInfo, renameEntry, renameObj, collectLabels,
                       encodeInfo, encodeStatic, encodeStr
                      )


main :: IO ()
main = link "link-output.js" =<< getArgs

link :: FilePath -> [FilePath] -> IO ()
link output inputs = do

  code <- forM inputs $ \input -> do
    collectCode =<< readObjectFile input

  let settings = mempty
      renamerState = emptyCompactorState
      dflags = undefined
      rtsDeps = S.fromList []

  let (renamerState', compacted, meta) = compact settings dflags renamerState (map funSymbol $ S.toList rtsDeps) (map (\(s,_,ci,si) -> (s,ci,si)) code)
      pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      renderedExports = TLE.encodeUtf8 . TL.fromStrict . T.unlines . filter (not . T.null) $ map (\(_,rs,_,_) -> rs) code

  BL.writeFile output $ mconcat rendered <> renderedMeta <> renderedExports

  where
    -- pkg           = depsPackage deps
    -- mod           = depsModule deps
    -- collectCode :: [ObjUnit] -> IO (_,_,_,_,_)
    collectCode l = let x = ( mconcat (map oiStat l)
                            , T.unlines (map oiRaw l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l)
                    in evaluate (rnf x) >> return x

compact :: GhcjsSettings
        -> DynFlags
        -> CompactorState
        -> [Text]
        -> [LinkedUnit]
        -> (CompactorState, [JStat], JStat)
compact settings dflags cs0 rtsDeps0 input0
--  | dumpHashes' input
  =
  let rtsDeps1 = rtsDeps0 ++
                 map (<> "_e") rtsDeps0 ++
                 map (<> "_con_e") rtsDeps0
      (cs1, input1) = packStrings cs0 input0
  in  renameInternals settings dflags cs1 rtsDeps1 input1

packStrings :: HasDebugCallStack
            => CompactorState
            -> [LinkedUnit]
            -> (CompactorState, [LinkedUnit])
packStrings cstate code =
  let allStatics :: [StaticInfo]
      allStatics = concatMap (\(_,_,x) -> x) code

      origStringTable :: StringTable
      origStringTable = cstate ^. stringTable

      allStrings :: Set ByteString
      allStrings = S.fromList $
                   filter (not . isExisting)
                          (mapMaybe (staticString . siVal) allStatics)
        where
          isExisting bs = isJust (HM.lookup bs $ stOffsets origStringTable)

      staticString :: StaticVal -> Maybe ByteString
      staticString (StaticUnboxed (StaticUnboxedString bs)) = Just bs
      staticString (StaticUnboxed (StaticUnboxedStringOffset bs)) = Just bs
      staticString _ = Nothing

      allStringsList :: [ByteString]
      allStringsList = S.toList allStrings

      -- we may see two kinds of null characters
      --   - string separator, packed as \0
      --   - within a string, packed as \cz\0
      -- we transform the strings to
      transformPackedLiteral :: Text -> Text
      transformPackedLiteral = T.concatMap f
        where
          f :: Char -> Text
          f '\0'  = "\^Z\0"
          f '\^Z' = "\^Z\^Z"
          f x     = T.singleton x

      allStringsPacked :: Text
      allStringsPacked = T.intercalate "\0" $
        map (\str -> maybe (packBase64 str)
                     transformPackedLiteral
                     (U.decodeModifiedUTF8 str))
            allStringsList

      packBase64 :: ByteString -> Text
      packBase64 bs
        | BS.null bs = mempty
        | otherwise =
          let (h,t) = BS.splitAt 128 bs
              esc   = T.singleton '\^Z' <>
                      T.singleton (chr . fromIntegral $ BS.length h + 0x1f)
              b64   = esc <> fromJust (U.decodeModifiedUTF8 (B64.encode h))
          in  maybe b64 transformPackedLiteral (U.decodeModifiedUTF8 h) <>
              packBase64 t

      allStringsWithOffset :: [(ByteString, Int)]
      allStringsWithOffset = snd $
        mapAccumL (\o b -> let o' = o + fromIntegral (BS.length b) + 1
                           in  o' `seq` (o', (b, o)))
                  0
                  allStringsList

      -- the offset of each of the strings in the big blob
      offsetIndex :: HashMap ByteString Int
      offsetIndex = HM.fromList allStringsWithOffset

      stringSymbol :: Ident
      stringSymbol = head $ cstate ^. identSupply

      stringSymbolT :: Text
      stringSymbolT = let (TxtI t) = stringSymbol in t

      stringSymbolIdx :: Int
      stringSymbolIdx = snd (bounds $ stTableIdents origStringTable) + 1

      -- append the new string symbol
      newTableIdents :: Array Int Text
      newTableIdents =
        listArray (0, stringSymbolIdx)
                  (elems (stTableIdents origStringTable) ++ [stringSymbolT])

      newOffsetsMap :: HashMap ByteString (Int, Int)
      newOffsetsMap = HM.union (stOffsets origStringTable)
                               (fmap (stringSymbolIdx,) offsetIndex)

      newIdentsMap :: HashMap Text (Either Int Int)
      newIdentsMap =
        let f (StaticInfo s (StaticUnboxed (StaticUnboxedString bs)) _)
              = Just (s, Left . fst $ newOffsetsMap HM.! bs)
            f (StaticInfo s (StaticUnboxed (StaticUnboxedStringOffset bs)) _)
              = Just (s, Right . snd $ newOffsetsMap HM.! bs)
            f _ = Nothing
        in HM.union (stIdents origStringTable)
                    (HM.fromList $ mapMaybe f allStatics)

      newStringTable :: StringTable
      newStringTable = StringTable newTableIdents newOffsetsMap newIdentsMap

      newOffsetsInverted :: HashMap (Int, Int) ByteString
      newOffsetsInverted = HM.fromList .
                           map (\(x,y) -> (y,x)) .
                           HM.toList $
                           newOffsetsMap

      replaceSymbol :: Text -> Maybe JVal
      replaceSymbol t =
        let f (Left i)  = JVar (TxtI $ newTableIdents ! i)
            f (Right o) = JInt (fromIntegral o)
        in  fmap f (HM.lookup t newIdentsMap)

      cstate0 :: CompactorState
      cstate0 = cstate & identSupply %~ tail
                       & stringTable .~ newStringTable

      initStr :: JStat
      initStr =
        DeclStat stringSymbol <>
        AssignStat (ValExpr $ JVar stringSymbol)
          (ApplExpr (ApplExpr (ValExpr $ JVar (TxtI "h$pstr"))
                              [ValExpr (JStr allStringsPacked)])
                    [])

      rewriteValsE :: JExpr -> JExpr
      rewriteValsE (ApplExpr e xs)
        | Just t <- appMatchStringLit e xs = ValExpr (JStr t)
      rewriteValsE (ValExpr v) = ValExpr (rewriteVals v)
      rewriteValsE e = e & exprsE %~ rewriteValsE

      rewriteVals :: JVal -> JVal
      rewriteVals (JVar (TxtI t))
        | Just v <- replaceSymbol t = v
      rewriteVals (JList es) = JList (map rewriteValsE es)
      rewriteVals (JHash m) = JHash (fmap rewriteValsE m)
      rewriteVals (JFunc args body) = JFunc args (body & exprsS %~ rewriteValsE)
      rewriteVals v = v

      rewriteStat :: JStat -> JStat
      rewriteStat st = st & exprsS %~ rewriteValsE

      appMatchStringLit :: JExpr -> [JExpr] -> Maybe Text
      appMatchStringLit (ValExpr (JVar (TxtI "h$decodeUtf8z")))
                        [ValExpr (JVar (TxtI x)), ValExpr (JVar (TxtI y))]
       | Just (Left i)  <- HM.lookup x newIdentsMap
       , Just (Right j) <- HM.lookup y newIdentsMap
       , Just bs        <- HM.lookup (i,j) newOffsetsInverted =
         U.decodeModifiedUTF8 bs
      appMatchStringLit _ _ = Nothing

      rewriteStatic :: StaticInfo -> Maybe StaticInfo
      rewriteStatic (StaticInfo _i
                                (StaticUnboxed StaticUnboxedString{})
                                _cc) =
        Nothing
      rewriteStatic (StaticInfo _i
                                (StaticUnboxed StaticUnboxedStringOffset {})
                                _cc) =
        Nothing
      rewriteStatic si = Just (si & staticInfoArgs %~ rewriteStaticArg)

      rewriteStaticArg :: StaticArg -> StaticArg
      rewriteStaticArg a@(StaticObjArg t) =
        case HM.lookup t newIdentsMap of
          Just (Right v)       -> StaticLitArg (IntLit $ fromIntegral v)
          Just (Left idx)      -> StaticObjArg (newTableIdents ! idx)
          _                    -> a
      rewriteStaticArg (StaticConArg v es)
        = StaticConArg v (map rewriteStaticArg es)
      rewriteStaticArg x = x

      initStatic :: LinkedUnit
      initStatic =
        let (TxtI ss) = stringSymbol
        in  (initStr, [], [StaticInfo ss (StaticThunk Nothing) Nothing])

      rewriteBlock :: LinkedUnit -> LinkedUnit
      rewriteBlock (stat, ci, si)
        = (rewriteStat stat, ci, mapMaybe rewriteStatic si)

    in (cstate0, initStatic : map rewriteBlock code)

renameInternals :: HasDebugCallStack
                => GhcjsSettings
                -> DynFlags
                -> CompactorState
                -> [Text]
                -> [LinkedUnit]
                -> (CompactorState, [JStat], JStat)
renameInternals _settings _dflags cs0 rtsDeps stats0a = (cs, stats, meta)
  where
    (stbs, stats0) = (if True -- gsDedupe settings
                      then dedupeBodies rtsDeps . dedupe rtsDeps
                      else (mempty,)) stats0a
    ((stats, meta), cs) = runState renamed cs0
    renamed :: State CompactorState ([JStat], JStat)
    renamed
      | True = do
      -- | buildingDebug dflags || buildingProf dflags = do
        cs <- get
        let renamedStats = map (\(s,_,_) -> s & identsS %~ lookupRenamed cs)
                               stats0
            statics      = map (renameStaticInfo cs)  $
                               concatMap (\(_,_,x) -> x) stats0
            infos        = map (renameClosureInfo cs) $
                               concatMap (\(_,x,_) -> x) stats0
            -- render metadata as individual statements
            meta = mconcat (map staticDeclStat statics) <>
                   (stbs & identsS %~ lookupRenamed cs) <>
                   mconcat (map (staticInitStat $ True {-buildingProf dflags-}) statics) <>
                   mconcat (map (closureInfoStat True) infos)
        return (renamedStats, meta)
      | otherwise = do
        -- collect all global objects and entries, add them to the renaming table
        mapM_ (\(_, cis, sis) -> do
               mapM_ (renameEntry . TxtI . ciVar) cis
               mapM_ (renameObj . siVar) sis
               mapM_ collectLabels sis) stats0

        -- sort our entries, store the results
        -- propagate all renamings throughtout the code
        cs <- get
        let -- Safari on iOS 10 (64 bit only?) crashes on very long arrays
            safariCrashWorkaround :: [Ident] -> JExpr
            safariCrashWorkaround xs =
              case chunksOf 10000 xs of
                (y:ys) | not (null ys)
                  -> ApplExpr (SelExpr (toJExpr y) (TxtI "concat"))
                              (map toJExpr ys)
                _ -> toJExpr xs
        let renamedStats = map (\(s,_,_) -> s & identsS %~ lookupRenamed cs)
                               stats0
            sortedInfo   = concatMap (\(_,xs,_) -> map (renameClosureInfo cs)
                                                       xs)
                                     stats0
            entryArr     = safariCrashWorkaround $
                           map (TxtI . fst) .
                           sortBy (compare `on` snd) .
                           HM.toList $
                           cs ^. entries
            lblArr       = map (TxtI . fst) .
                           sortBy (compare `on` snd) .
                           HM.toList $
                           cs ^. labels
            ss           = concatMap (\(_,_,xs) -> map (renameStaticInfo cs) xs)
                                     stats0
            infoBlock    = encodeStr (concatMap (encodeInfo cs) sortedInfo)
            staticBlock  = encodeStr (concatMap (encodeStatic cs) ss)
            stbs'        = stbs & identsS %~ lookupRenamed cs
            staticDecls  = mconcat (map staticDeclStat ss) <> stbs'
            meta = staticDecls #
                   appS "h$scheduleInit" [ entryArr
                                         , var "h$staticDelayed"
                                         , e lblArr
                                         , e infoBlock
                                         , e staticBlock
                                         ]
                   -- error "scheduleInit" 
                   {-
                   [j| h$scheduleInit( `entryArr`
                                     , h$staticDelayed
                                     , `lblArr`
                                     , `infoBlock`
                                     , `staticBlock`);
                       h$staticDelayed = [];
                     |] -}
        return (renamedStats, meta)

-- renderLinker :: GhcjsSettings
--              -> DynFlags
--              -> CompactorState
--              -> Set Fun
--              -> [(Package, Module, JStat, Text, [ClosureInfo], [StaticInfo], [ForeignRef])] -- ^ linked code per module
--              -> (BL.ByteString, Int64, CompactorState, LinkerStats)
-- renderLinker settings dflags renamerState rtsDeps code =
--   let (renamerState', compacted, meta) = Compactor.compact settings dflags renamerState (map funSymbol $ S.toList rtsDeps) (map (\(_,_,s,_,ci,si,_) -> (s,ci,si)) code)
--       pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
--       rendered  = parMap rdeepseq pe compacted
--       renderedMeta = pe meta
--       renderedExports = TLE.encodeUtf8 . TL.fromStrict . T.unlines . filter (not . T.null) $ map (\(_,_,_,rs,_,_,_) -> rs) code
--       mkStat (p,m,_,_,_,_,_) b = ((p,m), BL.length b)
--   in ( mconcat rendered <> renderedMeta <> renderedExports
--      , BL.length renderedMeta
--      , renamerState'
--      , M.fromList $ zipWith mkStat code rendered
--      )

-- collectDeps :: DynFlags
--             -> Map (Package, Module) (Deps, DepsLocation)
--             -> [InstalledUnitId]     -- ^ packages, code linked in this order
--             -> Set LinkableUnit -- ^ do not include these
--             -> Set Fun -- ^ roots
--             -> [LinkableUnit] -- ^ more roots
--             -> IO ( Set LinkableUnit
--                   , [(Package, Module, JStat, Text, [ClosureInfo], [StaticInfo], [ForeignRef])]
--                   )
-- collectDeps _dflags lookup packages base roots units = do
--   allDeps <- getDeps (fmap fst lookup) base roots units
--   -- read ghc-prim first, since we depend on that for static initialization
--   let packages' = uncurry (++) $ partition (==(toInstalledUnitId primUnitId)) (nub packages)
--       unitsByModule :: Map (Package, Module) IntSet
--       unitsByModule = M.fromListWith IS.union $
--                       map (\(p,m,n) -> ((p,m),IS.singleton n)) (S.toList allDeps)
--       lookupByPkg :: Map Package [(Deps, DepsLocation)]
--       lookupByPkg = M.fromListWith (++) (map (\((p,_m),v) -> (p,[v])) (M.toList lookup))
--   code <- fmap (catMaybes . concat) . forM packages' $ \pkg ->
--     mapM (uncurry $ extractDeps unitsByModule)
--          (fromMaybe [] $ M.lookup (mkPackage pkg) lookupByPkg)
--   return (allDeps, code)

-- readObjectFile :: FilePath -> IO [ObjUnit]
-- readObjectFile = readObjectFileKeys (\_ _ -> True)

-- readObjectFileKeys :: (Int -> [Text] -> Bool) -> FilePath -> IO [ObjUnit]
-- readObjectFileKeys p file = bracket (openBinaryFile file ReadMode) hClose $ \h -> do
--   mhdr <- getHeader <$> B.hGet h headerLength
--   case mhdr of
--     Left err -> error ("readObjectFileKeys: not a valid GHCJS object: " ++ file ++ "\n    " ++ err)
--     Right hdr -> do
--       bss <- B.hGet h (fromIntegral $ symbsLen hdr)
--       hSeek h RelativeSeek (fromIntegral $ depsLen hdr)
--       bsi <- B.fromStrict <$> BS.hGetContents h
--       return $ readObjectKeys' file p (getSymbolTable bss) bsi (B.drop (fromIntegral $ idxLen hdr) bsi)

-- extractDeps :: Map (Package, Module) IntSet
--             -> Deps
--             -> DepsLocation
--             -> IO (Maybe (Package, Module, JStat, Text, [ClosureInfo], [StaticInfo], [ForeignRef]))
-- extractDeps units deps loc =
--   case M.lookup (pkg, mod) units of
--     Nothing       -> return Nothing
--     Just modUnits -> do
--       let selector n _  = n `IS.member` modUnits || isGlobalUnit n
--       x <- case loc of
--         ObjectFile js_o  -> collectCode =<< readObjectFileKeys selector js_o
--         ArchiveFile js_a -> collectCode =<<
--                             (readObjectKeys (js_a ++ ':':T.unpack mod) selector <$>
--                             Ar.readObject (mkModuleName $ T.unpack mod) js_a)
--         InMemory n b     -> collectCode $
--                             readObjectKeys n selector (BL.fromStrict b)
--       evaluate (rnf x)
--       return x
--   where
--     pkg           = depsPackage deps
--     mod           = depsModule deps
--     collectCode l = let x = ( pkg
--                             , mod
--                             , mconcat (map oiStat l)
--                             , T.unlines (map oiRaw l)
--                             , concatMap oiClInfo l
--                             , concatMap oiStatic l
--                             , concatMap oiFImports l)
--                     in evaluate (rnf x) >> return (Just x)
