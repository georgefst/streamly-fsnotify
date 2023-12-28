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
    Event(..), StopWatching(stopWatching),
    -- * Events and predicates
    EventPredicate(..),
    everything, nothing, isDirectory, hasExtension, isCreation, isModification, isDeletion, isBasic, invert, conj, disj,
    -- * Watchers
    watchDirectory, watchDirectoryWith, watchTree, watchTreeWith,
) where

import Control.Arrow ((>>>))
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Semiring (Semiring(..))
import Streamly.Prelude (IsStream, MonadAsync)
import System.FilePath (isExtensionOf)
import System.FSNotify (
    ActionPredicate,
    Event (..),
    EventChannel,
    EventIsDirectory (IsDirectory),
    StopListening,
    WatchConfig,
    WatchManager,
    defaultConfig,
    startManagerConf,
    stopManager,
    watchDirChan,
    watchTreeChan,
 )

import qualified Streamly.Prelude as SP


-- | An object, which, when executed with 'stopWatching', stops a file system watch.
newtype StopWatching m = StopWatching { stopWatching :: m () }


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
isDirectory = EventPredicate $ eventIsDirectory >>> \case
    IsDirectory -> True
    _ -> False

-- | Allows through events triggered by file system entries with a specific
-- extension.
hasExtension :: FilePath -> EventPredicate
hasExtension fe = EventPredicate $ (fe `isExtensionOf`) . eventPath

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
isBasic = EventPredicate $ \e -> runPredicate isCreation e || runPredicate isModification e || runPredicate isDeletion e

-- | \'Flips\' the predicate - what it used to allow through is now blocked, and vice versa.
invert :: EventPredicate -> EventPredicate
invert (EventPredicate f) = EventPredicate $ not . f


{- Watchers -}

-- | Watch a given directory, but only at one level (thus, subdirectories will __not__ be watched recursively).
watchDirectory :: (IsStream t, MonadAsync m) => FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchDirectory = watch watchDirChan defaultConfig

-- | As 'watchDirectory', but with a specified set of watch options.
watchDirectoryWith :: (IsStream t, MonadAsync m) =>
    WatchConfig -> FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchDirectoryWith = watch watchDirChan

-- | Watch a given directory recursively (thus, subdirectories will also have their contents watched).
watchTree :: (IsStream t, MonadAsync m) => FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchTree = watch watchTreeChan defaultConfig

-- | As 'watchTree', but with a specified set of watch options.
watchTreeWith :: (IsStream t, MonadAsync m) => WatchConfig -> FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watchTreeWith = watch watchTreeChan


{- Util -}

watch :: (IsStream t, MonadAsync m) =>
    (WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening) ->
    WatchConfig -> FilePath -> EventPredicate -> m (StopWatching m, t m Event)
watch f conf p predicate = do
    manager <- liftIO $ startManagerConf conf
    let pred' = runPredicate predicate
    chan <- liftIO newChan
    stop <- liftIO $ f manager p pred' chan
    let reallyStop = StopWatching $ liftIO stop >> liftIO (stopManager manager)
    pure (reallyStop, SP.repeatM $ liftIO $ readChan chan)
