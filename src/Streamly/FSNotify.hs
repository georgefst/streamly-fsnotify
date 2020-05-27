{- |
__Introduction__

This provides file watching as a Streamly stream. You can either watch recursively (namely, a directory's contents and
all its subdirectories as well), or not. You can also filter out file system events you are not interested in. Lastly,
we provide a compositional scheme for constructing filters for file system events.

__Example__

This example program watches @\/home\/koz\/c-project@ (and any of its subdirectories) for added or modified files with a
@.c@ extension, and emits the change to the terminal, along with a timestamp of when it happened, forever:

> {-# LANGUAGE LambdaCase #-}
>
> import System.FilePath ((</>))
> import Streamly.FSNotify (EventPredicate, hasExtension, isDirectory, invert, isDeletion, conj, watchTree)
> import qualified Streamly.Prelude as SP
>
> -- conj -> both must be true
> -- invert -> true when the argument would be false and vice versa
> isCSourceFile :: EventPredicate
> isCSourceFile = hasExtension "c" `conj` invert isDirectory
>
> notDeletion :: EventPredicate
> notDeletion = invert isDeletion
>
> srcPath :: FilePath
> srcPath = "home" </> "koz" </> "c-project"
>
> -- first value given by watchTree stops the watcher
> -- we don't use it here, but if you want to, just call it
> main :: IO ()
> main = do
>     (_, stream) <- watchTree srcPath $ isCSourceFile `conj` notDeletion
>     SP.drain . SP.mapM go $ stream
>   where
>     go = \case
>         Added p t _ -> putStrLn $ "Created: " ++ show p ++ " at " ++ show t
>         Modified p t _ -> putStrLn $ "Modified: " ++ show p ++ " at " ++ show t
>         _ -> pure ()
-}
module Streamly.FSNotify (
    -- * Basic types
    FSEntryType(..), Event(..), StopWatching(stopWatching),
    eventPath, eventTime, eventFSEntry,
    -- * Events and predicates
    EventPredicate(..),
    everything, nothing, isDirectory, hasExtension, isCreation, isModification, isDeletion, isBasic, invert, conj, disj,
    -- * Watchers
    watchDirectory, watchDirectoryWith, watchTree, watchTreeWith,
) where

import Control.Arrow ((>>>))
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bool (bool)
import Data.Semiring (Semiring(..))
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Streamly (IsStream, MonadAsync)
import System.FilePath (isExtensionOf)

import qualified Streamly.Prelude as SP
import qualified System.FSNotify as FSN


-- | Allows us to designate 'Event's as being fired by a directory or a non-directory entry.
data FSEntryType = Dir | NotDir
    deriving (Eq, Show, Read, Bounded, Enum)

-- | A file system notification.
data Event
    = Added FilePath UTCTime FSEntryType -- ^ Creation event
    | Modified FilePath UTCTime FSEntryType -- ^ Modification event
    | Removed FilePath UTCTime FSEntryType -- ^ Deletion event
    | Other FilePath UTCTime Text -- ^ Some other kind of event
    deriving (Eq, Show)

-- | An object, which, when executed with 'stopWatching', stops a file system watch.
newtype StopWatching m = StopWatching { stopWatching :: m () }

-- | Helper to retrieve the file path associated with an event.
eventPath :: Event -> FilePath
eventPath = \case
    Added p _ _ -> p
    Modified p _ _ -> p
    Removed p _ _ -> p
    Other p _ _ -> p

-- | Helper to retrieve an event's timestamp.
eventTime :: Event -> UTCTime
eventTime = \case
    Added _ t _ -> t
    Modified _ t _ -> t
    Removed _ t _ -> t
    Other _ t _ -> t

-- | Helper to retrieve whether the event stems from a directory or not.
-- Returns 'Nothing' if the event is not \'basic\' (that is, not a creation, modification or deletion).
eventFSEntry :: Event -> Maybe FSEntryType
eventFSEntry = \case
    Added _ _ e -> Just e
    Modified _ _ e -> Just e
    Removed _ _ e -> Just e
    Other{} -> Nothing


{- Predicates -}

-- | A \'test\' for whether we want to \'notice\' an event. Should return 'True' for events we care about.
newtype EventPredicate = EventPredicate { runPredicate :: Event -> Bool }

{- | 'EventPredicate' can be a 'Semigroup' in two ways:

- Under logical conjunction (both of the conditions must be met); and
- Under logical disjunction (either of the conditions must be met).

Both of these can be made into a 'Monoid' by using the trivial predicate (always true) for the first case, and the null
predicate (always false) for the second. This makes it a valid candidate to be a semiring, which allows our users to
compose 'EventPredicate's using both of these methods, as they see fit.

If you want an instance of 'Semigroup' and 'Monoid' with one of these behaviours, you can use 'Data.Semiring.Add' (for
the logical disjunction behaviour) or 'Data.Semiring.Mul' (for the logical conjunction behaviour).
-}
instance Semiring EventPredicate where
    (EventPredicate f) `plus` (EventPredicate g) = EventPredicate $ (||) <$> f <*> g
    zero = nothing
    (EventPredicate f) `times` (EventPredicate g) = EventPredicate $ (&&) <$> f <*> g
    one = everything

-- | Predicate conjunction (meaning that /both/ have to be true for the result to be true).
-- Synonym for 'Data.Semigroup.times'.
conj :: EventPredicate -> EventPredicate -> EventPredicate
conj = times

-- | Predicate disjunction (meaning that /either/ has to be true for the result to be true).
-- Synonym for 'Data.Semigroup.plus'.
disj :: EventPredicate -> EventPredicate -> EventPredicate
disj = plus

-- | The trivial predicate (allows any event through).
everything :: EventPredicate
everything = EventPredicate $ const True

-- | The null predicate (allows no events through).
nothing :: EventPredicate
nothing = EventPredicate $ const False

-- | Allows through events that are caused by directories.
-- Note that this will assume that non-\'basic\' events (that is, not creations, modifications or deletions) do not stem
-- from directories; use with care.
isDirectory :: EventPredicate
isDirectory = EventPredicate $ eventFSEntry >>> \case
    Nothing -> False
    Just Dir -> True
    Just NotDir -> False

-- | Allows through events triggered by file system entries with a specific
-- extension.
hasExtension :: FilePath -> EventPredicate
hasExtension fe = EventPredicate $ (fe `isExtensionOf`) . \case
    Added p _ _ -> p
    Modified p _ _ -> p
    Removed p _ _ -> p
    Other p _ _ -> p

-- | Allows through only creation events.
isCreation :: EventPredicate
isCreation = EventPredicate $ \case
    Added{} -> True
    _ -> False

-- | Allows through only modification events.
isModification :: EventPredicate
isModification = EventPredicate $ \case
    Modified{} -> True
    _ -> False

-- | Allows through only deletion events.
isDeletion :: EventPredicate
isDeletion = EventPredicate $ \case
    Removed{} -> True
    _ -> False

-- | Allows through only \'basic\' events (namely creation, modification and deletion).
isBasic :: EventPredicate
isBasic = EventPredicate $ \case
    Other{} -> False
    _ -> True

-- | \'Flips\' the predicate - what it used to allow through is now blocked, and vice versa.
invert :: EventPredicate -> EventPredicate
invert (EventPredicate f) = EventPredicate $ not . f


{- Watchers -}

-- | Watch a given directory, but only at one level (thus, subdirectories will __not__ be watched recursively).
watchDirectory :: (IsStream t, MonadAsync m) => FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchDirectory = watch FSN.watchDirChan FSN.defaultConfig

-- | As 'watchDirectory', but with a specified set of watch options.
watchDirectoryWith :: (IsStream t, MonadAsync m) =>
    FSN.WatchConfig -> FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchDirectoryWith = watch FSN.watchDirChan

-- | Watch a given directory recursively (thus, subdirectories will also have their contents watched).
watchTree :: (IsStream t, MonadAsync m) => FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchTree = watch FSN.watchTreeChan FSN.defaultConfig

-- | As 'watchTree', but with a specified set of watch options.
watchTreeWith :: (IsStream t, MonadAsync m) => FSN.WatchConfig -> FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchTreeWith = watch FSN.watchTreeChan


{- Util -}

watch :: (IsStream t, MonadAsync m) =>
    (FSN.WatchManager -> FilePath -> FSN.ActionPredicate -> FSN.EventChannel -> IO FSN.StopListening) ->
    FSN.WatchConfig -> FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watch f conf p predicate = do
    manager <- liftIO $ FSN.startManagerConf conf
    let pred' = runPredicate predicate . mungeEvent
    chan <- liftIO newChan
    stop <- liftIO $ f manager p pred' chan
    let reallyStop = StopWatching $ liftIO stop >> liftIO (FSN.stopManager manager)
    pure (reallyStop, SP.repeatM $ liftIO $ mungeEvent <$> readChan chan)

mungeEvent :: FSN.Event -> Event
mungeEvent = \case
    FSN.Added p t b -> Added p t (isDir b)
    FSN.Modified p t b -> Modified p t (isDir b)
    FSN.Removed p t b -> Modified p t (isDir b)
    FSN.Unknown p t s -> Other p t (pack s)

isDir :: Bool -> FSEntryType
isDir = bool NotDir Dir
