{- |
__Introduction__

This provides file watching as a Streamly stream. You can either watch recursively (namely, a directory's contents and
all its subdirectories as well), or not. You can also filter out file system events you are not interested in. Lastly,
we provide a compositional scheme for constructing filters for file system events.

__Example__

This example program watches @\/home\/koz\/c-project@ (and any of its subdirectories) for added or modified files with a
@.c@ extension, and emits the change to the terminal, along with a timestamp of when it happened, forever:

> {\-# LANGUAGE LambdaCase #-\}
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
    watchDirectory,
    watchDirectoryWith,
    watchTree,
    watchTreeWith,
) where

import Control.Concurrent.Chan (newChan, readChan)
import Streamly.Prelude (IsStream)
import System.FSNotify (
    ActionPredicate,
    Event (..),
    EventChannel,
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

-- | Watch a given directory, but only at one level (thus, subdirectories will __not__ be watched recursively).
watchDirectory :: (IsStream t) => FilePath -> ActionPredicate -> IO (StopListening, t IO Event)
watchDirectory = watchDirectoryWith defaultConfig

-- | As 'watchDirectory', but with a specified set of watch options.
watchDirectoryWith :: (IsStream t) => WatchConfig -> FilePath -> ActionPredicate -> IO (StopListening, t IO Event)
watchDirectoryWith = watch watchDirChan

-- | Watch a given directory recursively (thus, subdirectories will also have their contents watched).
watchTree :: (IsStream t) => FilePath -> ActionPredicate -> IO (StopListening, t IO Event)
watchTree = watchTreeWith defaultConfig

-- | As 'watchTree', but with a specified set of watch options.
watchTreeWith :: (IsStream t) => WatchConfig -> FilePath -> ActionPredicate -> IO (StopListening, t IO Event)
watchTreeWith = watch watchTreeChan

watch ::
    (IsStream t) =>
    (WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening) ->
    WatchConfig ->
    FilePath ->
    ActionPredicate ->
    IO (StopListening, t IO Event)
watch f conf p predicate = do
    manager <- startManagerConf conf
    chan <- newChan
    stop <- f manager p predicate chan
    pure (stop >> stopManager manager, SP.repeatM $ readChan chan)
