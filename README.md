## What's the deal with this library?

[``streamly``][1] is an undoubtedly awesome library - [fast][2], flexible, and
well-documented. File system watching is a natural fit for a streaming library,
and this is exactly what ``streamly-fsnotify`` provides you.

As an example, here is a program which watches ``/home/gthomas/c-project/`` and any
of its subdirectories for added or modified C source files (which we take to be
anything with a ``.c`` extension). This program then writes that the event
occurred, to what file, and when, forever.

```haskell
{-# LANGUAGE GHC2021, BlockArguments, LambdaCase #-}

import Data.Functor.Contravariant (Predicate (Predicate))
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as SP
import System.FilePath (isExtensionOf, (</>))

import Streamly.FSNotify

isCSourceFile :: Predicate Event
isCSourceFile = Predicate \e ->
    "c" `isExtensionOf` eventPath e && eventIsDirectory e == IsFile

notDeletion :: Predicate Event
notDeletion = Predicate \case
    Removed{} -> False
    _ -> True

srcPath :: FilePath
srcPath = "/" </> "home" </> "gthomas" </> "c-project"

main :: IO ()
main = SP.fold (SF.drainMapM go) $ watchTree srcPath $ isCSourceFile <> notDeletion
  where
    go = \case
        Added p t _ -> putStrLn $ "Created: " ++ show p ++ " at " ++ show t
        Modified p t _ -> putStrLn $ "Modified: " ++ show p ++ " at " ++ show t
        _ -> pure ()
```

## That seems pretty cool! What kind of features can I expect?

* Cross-platform - should work anywhere both ``streamly`` and ``fsnotify`` do.
* Efficient (event-driven, so won't shred your CPU or load your RAM).
* Able to do one-level and recursive watching.

## Sounds good? Can I use it?

We've test-built this library for GHCs 8.6.5 through 8.10.1 on GNU/Linux. In
theory, ``streamly-fsnotify`` should work everywhere both ``streamly`` and
``fsnotify`` will, which includes other OSes (such as Windows). However, we
haven't tried it ourselves - let us know if you do!

[1]: http://hackage.haskell.org/package/streamly
[2]: https://github.com/composewell/streaming-benchmarks
