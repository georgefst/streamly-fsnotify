{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Streamly.FSNotify where

import Data.Semiring (Semiring(..))
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Streamly (IsStream, MonadAsync)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bool (bool)
import Data.Time.Clock (UTCTime)
import Data.Text (Text, pack)
import System.Path (Path, FsRoot, FsPath(..), FileExt,
                    isExtensionOf, fromFilePath, toFilePath, makeAbsolute)

import qualified Streamly.Prelude as SP
import qualified System.FSNotify as FSN

data FSEntryType = Dir | NotDir
  deriving (Eq, Show, Read, Bounded, Enum)

data Event where
  Added :: (FsRoot root) => Path root -> UTCTime -> FSEntryType -> Event
  Modified :: (FsRoot root) => Path root -> UTCTime -> FSEntryType -> Event
  Removed :: (FsRoot root) => Path root -> UTCTime -> FSEntryType -> Event
  Other :: (FsRoot root) => Path root -> UTCTime -> Text -> Event

type StopWatching = IO ()

{-# INLINE eventPath #-}
eventPath :: Event -> FsPath
eventPath (Added p _ _) = FsPath p
eventPath (Modified p _ _) = FsPath p
eventPath (Removed p _ _) = FsPath p
eventPath (Other p _ _) = FsPath p

{-# INLINE eventTime #-}
eventTime :: Event -> UTCTime
eventTime (Added _ t _) = t
eventTime (Modified _ t _) = t
eventTime (Removed _ t _) = t
eventTime (Other _ t _) = t

{-# INLINE eventFSEntry #-}
eventFSEntry :: Event -> Maybe FSEntryType
eventFSEntry (Added _ _ e) = Just e
eventFSEntry (Modified _ _ e) = Just e
eventFSEntry (Removed _ _ e) = Just e
eventFSEntry Other{} = Nothing

-- Predicates

newtype EventPredicate = EventPredicate { runPredicate :: Event -> Bool }

instance Semiring EventPredicate where
  {-# INLINE plus #-}
  (EventPredicate f) `plus` (EventPredicate g) = EventPredicate ((||) <$> f <*> g)
  {-# INLINE zero #-}
  zero = nothing
  {-# INLINE times #-}
  (EventPredicate f) `times` (EventPredicate g) = EventPredicate ((&&) <$> f <*> g)
  {-# INLINE one #-}
  one = everything

{-# INLINE everything #-}
everything :: EventPredicate
everything = EventPredicate . const $ True

{-# INLINE nothing #-}
nothing :: EventPredicate
nothing = EventPredicate . const $ False

{-# INLINE isDirectory #-}
isDirectory :: EventPredicate
isDirectory = EventPredicate $ \e -> case eventFSEntry e of
  Nothing -> False
  Just Dir -> True
  Just NotDir -> False

{-# INLINE hasExtension #-}
hasExtension :: FileExt -> EventPredicate
hasExtension fe = EventPredicate $ \case 
  (Added p _ _) -> isExtensionOf fe p
  (Modified p _ _) -> isExtensionOf fe p
  (Removed p _ _) -> isExtensionOf fe p
  (Other p _ _) -> isExtensionOf fe p

{-# INLINE isCreation #-}
isCreation :: EventPredicate
isCreation = EventPredicate $ \case
  Added{} -> True
  _ -> False

{-# INLINE isModification #-}
isModification :: EventPredicate
isModification = EventPredicate $ \case
  Modified{} -> True
  _ -> False

{-# INLINE isDeletion #-}
isDeletion :: EventPredicate
isDeletion = EventPredicate $ \case
  Removed{} -> True
  _ -> False

{-# INLINE isBasic #-}
isBasic :: EventPredicate
isBasic = EventPredicate $ \case
  Other{} -> False
  _ -> True

{-# INLINE invert #-}
invert :: EventPredicate -> EventPredicate
invert (EventPredicate f) = EventPredicate (not . f)

-- Watchers

{-# INLINE watchDirectory #-}
watchDirectory :: (IsStream t, MonadAsync m) => FsPath -> EventPredicate -> m (StopWatching, t m Event)
watchDirectory = watch FSN.watchDirChan 

{-# INLINE watchTree #-}
watchTree :: (IsStream t, MonadAsync m) => FsPath -> EventPredicate -> m (StopWatching, t m Event)
watchTree = watch FSN.watchTreeChan

-- Helpers
{-# INLINE watch #-}
watch :: (IsStream t, MonadAsync m) => 
  (FSN.WatchManager -> FilePath -> (FSN.Event -> Bool) -> Chan FSN.Event -> IO (IO ())) -> 
  FsPath -> EventPredicate -> m (StopWatching, t m Event)
watch f p predicate = do
  manager <- liftIO FSN.startManager
  fp <- toFilePath <$> (liftIO . makeAbsolute $ p)
  let pred' = runPredicate predicate . mungeEvent
  chan <- liftIO newChan
  stop <- liftIO . f manager fp pred' $ chan
  let reallyStop = stop >> FSN.stopManager manager
  pure (reallyStop, SP.repeatM (liftIO . fmap mungeEvent . readChan $ chan))

{-# INLINE mungeEvent #-}
mungeEvent :: FSN.Event -> Event
mungeEvent e = case fromFilePath . FSN.eventPath $ e of
  (FsPath p) -> case e of
                  FSN.Added _ t b -> Added p t (isDir b)
                  FSN.Modified _ t b -> Modified p t (isDir b)
                  FSN.Removed _ t b -> Removed p t (isDir b)
                  FSN.Unknown _ t s -> Other p t (pack s)
  where isDir = bool NotDir Dir
