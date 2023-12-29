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
    watchDir,
    watchTree,
) where

import Control.Concurrent.Chan (newChan, readChan)
import Data.Functor.Contravariant (Predicate (getPredicate))
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.StreamK qualified as SK
import System.FSNotify (
    ActionPredicate,
    Event (..),
    EventChannel,
    StopListening,
    WatchManager,
    defaultConfig,
    startManagerConf,
    stopManager,
    watchDirChan,
    watchTreeChan,
 )

-- | Watch a given directory, but only at one level (thus, subdirectories will __not__ be watched recursively).
watchDir :: FilePath -> Predicate Event -> Stream IO Event
watchDir = watch watchDirChan

-- | Watch a given directory recursively (thus, subdirectories will also have their contents watched).
watchTree :: FilePath -> Predicate Event -> Stream IO Event
watchTree = watch watchTreeChan

watch ::
    (WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening) ->
    FilePath ->
    Predicate Event ->
    Stream IO Event
watch f p predicate = withInit
    do
        manager <- startManagerConf defaultConfig
        chan <- newChan
        stop <- f manager p (getPredicate predicate) chan
        pure (chan, stop >> stopManager manager)
    \(chan, stop) -> S.finally stop $ S.repeatM $ readChan chan
  where
    -- TODO a few problems with this:
    -- it's vendored from `georgefst-utils`
    -- it's inelegant and inefficient (though inhibiting fusion isn't a major issue since we already have `finally`)
    -- it incurs a direct dependency on `streamly-core`
    withInit init_ stream =
        SK.toStream . SK.unCross $
            SK.mkCross . SK.fromStream . stream
                =<< SK.mkCross (SK.fromStream $ S.fromEffect init_)
