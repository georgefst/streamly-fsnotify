# ``streamly-fsnotify``

## What's the deal with this library?

[``streamly``][1] is an undoubtedly awesome library - [fast][2], flexible, and
well-documented. File system watching is a natural fit for a streaming library,
and this is exactly what ``streamly-notify`` provides you.

As an example, here is a program which watches ``/home/koz/c-project/`` and any
of its subdirectories for added or modified C source files (which we take to be
anything with a ``.c`` extension). This program then writes that the event
occurred, to what file, and when, forever.

```haskell

{-# LANGUAGE LambdaCase #-}

import Streamly.FSNotify (EventPredicate,
                          hasExtension, is Directory, invert, isDeletion,
                          watchTree)
import System.Path (FsPath, FileExt, fromFilePath)
import Data.Semiring (times)

import qualified Streamly.Prelude as SP

-- times -> both must be true
-- invert -> true when the argument would be false and vice versa
isCSourceFile :: EventPredicate
isCSourceFile = hasExtension (FileExt "c") `times` (invert isDirectory)

notDeletion :: EventPredicate
notDeletion = invert isDeletion

srcPath :: FsPath
srcPath = fromFilePath "/home/koz/c-project"

-- first value given by watchTree stops the watcher
-- we don't use it here, but if you want to, just call it
main :: IO ()
main = do (_, stream) <- watchTree srcPath (isCSourceFile `times` notDeletion)
          SP.drain . SP.mapM go $ stream
  where go = \case (Added p t _) -> putStrLn ("Created: " ++ show p ++ " at " ++ show t)
                   (Modified p t _) -> putStrLn ("Modified: " ++ show p ++ " at " ++ show t)
                   _ -> pure ()
```

## That seems pretty cool! What kind of features can I expect?

* Cross-platform - should work anywhere both ``streamly`` and ``fsnotify`` do.
* Efficient (event-driven, so won't shred your CPU or load your RAM).
* Able to do one-level and recursive watching.
* Extensive set of filtering predicates, so you don't have to see events you
  don't care about!
* Compositional and principled treatment of file paths and event filtering
  predicates.

## Sounds good? Can I use it?

We've test-built this library for GHCs 8.2.2 through 8.8.1 on GNU/Linux. In
theory, ``streamly-fsnotify`` should work everywhere both ``streamly`` and
``fsnotify`` will, which includes older GHCs (7.10) and other OSes (such as
Windows). However, we haven't tried it ourselves - let us know if you do!

## License

This library is under the GNU General Public License, version 3 or later (SPDX
code ``GPL-3.0-or-later``). For more details, see the ``LICENSE.md`` file.

[1]: http://hackage.haskell.org/package/streamly
[2]: https://github.com/composewell/streaming-benchmarks
