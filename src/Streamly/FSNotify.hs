{- |
__Example__

Here is a program which watches @\/home\/georgefst\/c-project@ and any of its subdirectories for added or modified
C source files (which we take to be anything with a @.c@ extension). This program then writes that the event occurred,
to what file, and when, forever.

> {\-# LANGUAGE GHC2021, BlockArguments, LambdaCase #-\}
>
> import Streamly.Data.Fold qualified as SF
> import Streamly.Data.Stream.Prelude qualified as SP
> import System.FilePath (isExtensionOf, (</>))
>
> import Streamly.FSNotify
>
> isCSourceFile :: Event -> Bool
> isCSourceFile e =
>     "c" `isExtensionOf` eventPath e && eventIsDirectory e == IsFile
>
> srcPath :: FilePath
> srcPath = "/" </> "home" </> "gthomas" </> "c-project"
>
> main :: IO ()
> main = SP.fold (SF.drainMapM go) $ watchTree srcPath
>   where
>     go = \case
>         e | not (isCSourceFile e) -> pure ()
>         Added p t _ -> putStrLn $ "Created: " ++ show p ++ " at " ++ show t
>         Modified p t _ -> putStrLn $ "Modified: " ++ show p ++ " at " ++ show t
>         _ -> pure ()
-}
module Streamly.FSNotify (
    Event (..),
    EventIsDirectory (..),
    watchDir,
    watchTree,
) where

import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (liftIO)
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.StreamK qualified as SK
import System.FSNotify (
    ActionPredicate,
    Event (..),
    EventChannel,
    EventIsDirectory (..),
    StopListening,
    WatchManager,
    defaultConfig,
    startManagerConf,
    stopManager,
    watchDirChan,
    watchTreeChan,
 )

-- | Watch a given directory, but only at one level (thus, subdirectories will __not__ be watched recursively).
watchDir :: (MonadAsync m, MonadCatch m) => FilePath -> Stream m Event
watchDir = watch watchDirChan

-- | Watch a given directory recursively (thus, subdirectories will also have their contents watched).
watchTree :: (MonadAsync m, MonadCatch m) => FilePath -> Stream m Event
watchTree = watch watchTreeChan

watch ::
    (MonadAsync m, MonadCatch m) =>
    (WatchManager -> FilePath -> ActionPredicate -> EventChannel -> IO StopListening) ->
    FilePath ->
    Stream m Event
watch f p = withInit
    do
        manager <- liftIO $ startManagerConf defaultConfig
        chan <- liftIO newChan
        stop <- liftIO $ f manager p (const True) chan
        pure (chan, liftIO $ stop >> liftIO (stopManager manager))
    \(chan, stop) -> S.finally stop $ S.repeatM $ liftIO $ readChan chan
  where
    -- TODO a few problems with this:
    -- it's vendored from `georgefst-utils`
    -- it's inelegant and inefficient (though inhibiting fusion isn't a major issue since we already have `finally`)
    -- it incurs a direct dependency on `streamly-core`
    withInit init_ stream =
        SK.toStream . SK.unCross $
            SK.mkCross . SK.fromStream . stream
                =<< SK.mkCross (SK.fromStream $ S.fromEffect init_)
