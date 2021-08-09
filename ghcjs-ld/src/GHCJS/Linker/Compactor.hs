{-# LANGUAGE
  FlexibleContexts,
  OverloadedStrings,
  PatternGuards,
  ScopedTypeVariables,
  TupleSections
#-}

module GHCJS.Linker.Compactor where

-- Utilities
import           Control.Lens              hiding ((#), argument)
import           Control.Monad.State
import           Data.Array
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base64    as B64
import           Data.Char                 (chr)
import           Data.Function             (on)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import qualified Data.Text                 as T

-- GHC
import Util

-- GHCJS
import Compiler.JMacro.Base
import Compiler.JMacro.Combinators
import Gen2.Base
import Gen2.ClosureInfo
import qualified Gen2.Utils as U
import Gen2.Compactor (LinkedUnit, exprsE, exprsS, identsS, lookupRenamed, staticInfoArgs, dedupeBodies,
                       dedupe, renameStaticInfo, renameClosureInfo,
                       encodeInfo, encodeStatic, encodeStr
                      )

-- GHCJS-LD
import GHCJS.Linker.Monad


compact :: Linker m
        => [Text]
        -> [LinkedUnit]
        -> m ([JStat], JStat)
compact rtsDeps0 input = do
--  | dumpHashes' input
    let rtsDeps1 = rtsDeps0 ++
                   map (<> "_e") rtsDeps0 ++
                   map (<> "_con_e") rtsDeps0
    renameInternals rtsDeps1 =<< packStrings input

packStrings :: (MonadState CompactorState m, HasDebugCallStack)
            => [LinkedUnit]
            -> m [LinkedUnit]
packStrings code = do
  
  let allStatics :: [StaticInfo]
      allStatics = concatMap (\(_,_,x) -> x) code

  origStringTable :: StringTable <- use stringTable

  let isExisting bs = isJust (HM.lookup bs $ stOffsets origStringTable)

  let allStrings :: Set ByteString
      allStrings = S.fromList $
                   filter (not . isExisting)
                          (mapMaybe (staticString . siVal) allStatics)

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

  stringSymbol :: Ident <- head <$> use identSupply

  let stringSymbolT :: Text
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

  identSupply %= tail
  stringTable .= StringTable newTableIdents newOffsetsMap newIdentsMap

  return $ initStatic : map rewriteBlock code

renameInternals :: (Linker m, HasDebugCallStack)
                => [Text]
                -> [LinkedUnit]
                -> m ([JStat], JStat)
renameInternals rtsDeps stats0a = do
  ddupe <- linkDedupe
  let (stbs, stats0) = (if ddupe
                        then dedupeBodies rtsDeps . dedupe rtsDeps
                        else (mempty,)) stats0a
  renamed stbs stats0 <$> linkProf <*> linkDebug >>= id
  where
    renamed :: MonadState CompactorState m
            => JStat
            -> [(JStat, [ClosureInfo], [StaticInfo])]
            -> Bool
            -> Bool
            -> m ([JStat], JStat)
    renamed stbs stats0 prof debug
      | prof || debug = do
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
                   mconcat (map (staticInitStat prof) statics) <>
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

renameObj :: MonadState CompactorState m
          => Text
          -> m Text
renameObj xs = do
  (TxtI xs') <- renameVar (TxtI xs)
  addItem statics statics numStatics numStatics parentStatics xs'
  return xs'

renameEntry :: MonadState CompactorState m
            => Ident
            -> m Ident
renameEntry i = do
  i'@(TxtI i'') <- renameVar i
  addItem entries entries numEntries numEntries parentEntries i''
  return i'

renameVar :: MonadState CompactorState m
          => Ident   -- ^ text identifier to rename
          -> m Ident -- ^ the updated renamer state and the new ident
renameVar i@(TxtI t)
  | "h$$" `T.isPrefixOf` t = do
      m <- use nameMap
      case HM.lookup t m of
        Just r  -> return r
        Nothing -> do
          y <- newIdent
          nameMap %= HM.insert t y
          return y
  | otherwise = return i

addItem :: (MonadState CompactorState m, HasDebugCallStack)
        => Getting (HashMap Text Int) CompactorState (HashMap Text Int)
        -> Setting (->)
                   CompactorState
                   CompactorState
                   (HashMap Text Int)
                   (HashMap Text Int)
        -> Getting Int CompactorState Int
        -> ASetter' CompactorState Int
        -> Getting (HashMap Text Int) CompactorState (HashMap Text Int)
        -> Text
        -> m ()
addItem items items' numItems numItems' parentItems i = do
  s <- use items
  case HM.lookup i s of
    Just _ -> return ()
    Nothing -> do
      sp <- use parentItems
      case HM.lookup i sp of
        Just _  -> return ()
        Nothing -> do
          ni <- use numItems
          items' %= HM.insert i ni
          numItems' += 1

collectLabels :: MonadState CompactorState m => StaticInfo -> m ()
collectLabels si = mapM_ (addItem labels labels numLabels numLabels parentLabels)
                         (labelsV . siVal $ si)
  where
    labelsV (StaticData _ args) = concatMap labelsA args
    labelsV (StaticList args _) = concatMap labelsA args
    labelsV _                   = []
    labelsA (StaticLitArg l) = labelsL l
    labelsA _                = []
    labelsL (LabelLit _ lbl) = [lbl]
    labelsL _                = []

newIdent :: MonadState CompactorState m
         => m Ident
newIdent = do
  yys <- use identSupply
  case yys of
    (y:ys) -> do
      identSupply .= ys
      return y
    _ -> error "newIdent: empty list"
