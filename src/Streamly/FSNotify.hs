{-
 - Copyright (C) 2019  Koz Ross <koz.ross@retro-freedom.nz>
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module:        Streamly.FSNotify
-- Description:   Filesystem watching as Streamly streams. 
-- Copyright:     (C) Koz Ross 2019
-- License:       GPL version 3.0 or later
-- Maintainer:    koz.ross@retro-freedom.nz
-- Stability:     Experimental
-- Portability:   GHC only
--
--
module Streamly.FSNotify 
(
  -- * Basic types
  FSEntryType(..), Event(..), StopWatching,
  eventPath, eventTime, eventFSEntry,
  -- * Events and predicates
  EventPredicate(..), 
  isDirectory, hasExtension, isCreation, isModification, isDeletion, isBasic, invert,
  -- * Watchers
  watchDirectory, watchDirectoryWith, watchTree, watchTreeWith
) where

import Data.Semiring (Semiring(..))
import Control.Concurrent.Chan (newChan, readChan)
import Streamly (IsStream, MonadAsync)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bool (bool)
import Data.Time.Clock (UTCTime)
import Data.Text (Text, pack)
import System.Path (Path, FsPath(..), FileExt, Absolute,
                    isExtensionOf, toFilePath, makeAbsolute, fromAbsoluteFilePath)

import qualified Streamly.Prelude as SP
import qualified System.FSNotify as FSN

-- | Allows us to designate 'Event's as being fired by a directory or a
-- non-directory entry.
data FSEntryType = Dir | NotDir
  deriving (Eq, Show, Read, Bounded, Enum)

-- | A file system notification.
data Event = Added (Path Absolute) UTCTime FSEntryType -- ^ Creation event
           | Modified (Path Absolute) UTCTime FSEntryType -- ^ Modification event
           | Removed (Path Absolute) UTCTime FSEntryType -- ^ Deletion event
           | Other (Path Absolute) UTCTime Text -- ^ Some other kind of event
  deriving (Eq, Show)

-- | A function, which, when executed, stops a file system watch.
type StopWatching m = m ()

-- | Helper to retrieve the file path associated with an event.
{-# INLINE eventPath #-}
eventPath :: Event -> FsPath
eventPath (Added p _ _) = FsPath p
eventPath (Modified p _ _) = FsPath p
eventPath (Removed p _ _) = FsPath p
eventPath (Other p _ _) = FsPath p

-- | Helper to retrieve an event's timestamp.
{-# INLINE eventTime #-}
eventTime :: Event -> UTCTime
eventTime (Added _ t _) = t
eventTime (Modified _ t _) = t
eventTime (Removed _ t _) = t
eventTime (Other _ t _) = t

-- | Helper to retrieve whether the event stems from a directory or not.
-- Returns 'Nothing' if the event is not \'basic\' (that is, not a creation,
-- modification or deletion).
{-# INLINE eventFSEntry #-}
eventFSEntry :: Event -> Maybe FSEntryType
eventFSEntry (Added _ _ e) = Just e
eventFSEntry (Modified _ _ e) = Just e
eventFSEntry (Removed _ _ e) = Just e
eventFSEntry Other{} = Nothing

-- Predicates

-- | A \'test\' for whether we want to \'notice\' an event. Should return 'True'
-- for events we care about.
newtype EventPredicate = EventPredicate { runPredicate :: Event -> Bool }

-- | 'EventPredicate' can be a 'Semigroup' in two ways:
--
-- - Under logical conjunction (both of the conditions must be met); and
-- - Under logical disjunction (either of the conditions must be met).
--
-- Both of these can be made into a 'Monoid' by using the trivial predicate
-- (always true) for the first case, and the null predicate (always false) for
-- the second. This makes it a valid candidate to be a semiring, which allows
-- our users to compose 'EventPredicate's using both of these methods, as they
-- see fit.
--
-- If you want an instance of 'Semigroup' and 'Monoid' with one of these
-- behaviours, you can use 'Data.Semiring.Add' (for the logical disjunction
-- behaviour) or 'Data.Semiring.Mul' (for the logical conjunction behaviour).
instance Semiring EventPredicate where
  {-# INLINE plus #-}
  (EventPredicate f) `plus` (EventPredicate g) = EventPredicate ((||) <$> f <*> g)
  {-# INLINE zero #-}
  zero = nothing
  {-# INLINE times #-}
  (EventPredicate f) `times` (EventPredicate g) = EventPredicate ((&&) <$> f <*> g)
  {-# INLINE one #-}
  one = everything

-- | The trivial predicate (allows any event through).
{-# INLINE everything #-}
everything :: EventPredicate
everything = EventPredicate . const $ True

-- | The null predicate (allows no events through).
{-# INLINE nothing #-}
nothing :: EventPredicate
nothing = EventPredicate . const $ False

-- | Allows through events that are caused by directories.
-- Note that this will assume that non-\'basic\' events (that is, not creations,
-- modifications or deletions) do not stem from directories; use with care.
{-# INLINE isDirectory #-}
isDirectory :: EventPredicate
isDirectory = EventPredicate $ \e -> case eventFSEntry e of
  Nothing -> False
  Just Dir -> True
  Just NotDir -> False

-- | Allows through events triggered by file system entries with a specific
-- extension.
{-# INLINE hasExtension #-}
hasExtension :: FileExt -> EventPredicate
hasExtension fe = EventPredicate $ \case 
  (Added p _ _) -> isExtensionOf fe p
  (Modified p _ _) -> isExtensionOf fe p
  (Removed p _ _) -> isExtensionOf fe p
  (Other p _ _) -> isExtensionOf fe p

-- | Allows through only creation events.
{-# INLINE isCreation #-}
isCreation :: EventPredicate
isCreation = EventPredicate $ \case
  Added{} -> True
  _ -> False

-- | Allows through only modification events.
{-# INLINE isModification #-}
isModification :: EventPredicate
isModification = EventPredicate $ \case
  Modified{} -> True
  _ -> False

-- | Allows through only deletion events.
{-# INLINE isDeletion #-}
isDeletion :: EventPredicate
isDeletion = EventPredicate $ \case
  Removed{} -> True
  _ -> False

-- | Allows through only \'basic\' events (namely creation, modification and
-- deletion).
{-# INLINE isBasic #-}
isBasic :: EventPredicate
isBasic = EventPredicate $ \case
  Other{} -> False
  _ -> True

-- | \'Flips\' the predicate - what it used to allow through is now blocked, and
-- vice versa.
{-# INLINE invert #-}
invert :: EventPredicate -> EventPredicate
invert (EventPredicate f) = EventPredicate (not . f)

-- Watchers

-- | Watch a given directory, but only at one level (thus, subdirectories will
-- __not__ be watched recursively).
{-# INLINE watchDirectory #-}
watchDirectory :: (IsStream t, MonadAsync m) => FsPath -> EventPredicate -> m (StopWatching m, t m Event)
watchDirectory = watch FSN.watchDirChan FSN.defaultConfig 

-- | As 'watchDirectory', but with a specified set of watch options.
{-# INLINE watchDirectoryWith #-}
watchDirectoryWith :: (IsStream t, MonadAsync m) => FSN.WatchConfig -> FsPath -> EventPredicate -> m (StopWatching m, t m Event)
watchDirectoryWith = watch FSN.watchDirChan

-- | Watch a given directory recursively (thus, subdirectories will also have
-- their contents watched).
{-# INLINE watchTree #-}
watchTree :: (IsStream t, MonadAsync m) => FsPath -> EventPredicate -> m (StopWatching m, t m Event)
watchTree = watch FSN.watchTreeChan FSN.defaultConfig

-- | As 'watchTree', but with a specified set of watch options.
{-# INLINE watchTreeWith #-}
watchTreeWith :: (IsStream t, MonadAsync m) => FSN.WatchConfig -> FsPath -> EventPredicate -> m (StopWatching m, t m Event)
watchTreeWith = watch FSN.watchTreeChan

-- Helpers
{-# INLINE watch #-}
watch :: (IsStream t, MonadAsync m) => 
  (FSN.WatchManager -> FilePath -> FSN.ActionPredicate -> FSN.EventChannel -> IO FSN.StopListening) -> 
  FSN.WatchConfig ->
  FsPath -> EventPredicate -> m (StopWatching m, t m Event)
watch f conf p predicate = do
  manager <- liftIO . FSN.startManagerConf $ conf
  fp <- toFilePath <$> (liftIO . makeAbsolute $ p)
  let pred' = runPredicate predicate . mungeEvent
  chan <- liftIO newChan
  stop <- liftIO . f manager fp pred' $ chan
  let reallyStop = liftIO stop >> liftIO (FSN.stopManager manager)
  pure (reallyStop, SP.repeatM (liftIO . fmap mungeEvent . readChan $ chan))

{-# INLINE mungeEvent #-}
mungeEvent :: FSN.Event -> Event
mungeEvent = \case
  (FSN.Added p t b) -> Added (fromAbsoluteFilePath p) t (isDir b)
  (FSN.Modified p t b) -> Modified (fromAbsoluteFilePath p) t (isDir b)
  (FSN.Removed p t b) -> Modified (fromAbsoluteFilePath p) t (isDir b)
  (FSN.Unknown p t s) -> Other (fromAbsoluteFilePath p) t (pack s)

{-# INLINE isDir #-}
isDir :: Bool -> FSEntryType
isDir = bool NotDir Dir
