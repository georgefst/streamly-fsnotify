# Revision history for streamly-fsnotify

## 2.1 -- 2023-12-29

- Remove file predicates as they're actually pointless. They do perhaps make sense in the context of `fsnotify` itself due to its approach to concurrency and action handling. But for this library, ignoring events via a predicate isn't really any different to just choosing not to do anything when it comes to matching on the received events. I had implicitly assumed when taking over this library that the predicates were smarter than that, telling upstream systems like `inotify` to totally ignore certain events. But that wouldn't have been possible with the old API, and would need support from `fsnotify` anyway, which may not be possible due to platform inconsistencies.

## 2.0 -- 2023-12-29

With Streamly also making major breaking changes, this seemed like the time for a radical overhaul (we needed to move to the new stream type _at some point_, and that would break everything anyway). Essentially, having taken over this package nearly four years ago, and used it extensively, I've decided that it was unnecessarily complex. The line count is now _much_ shorter, but the interesting stuff is still here. In particular:

- The predicate algebra, while somewhat neat, didn't really seem to belong in a file-watching library.
- Trying to fully abstract over `fsnotify` was annoying in practice, both as a maintainer and a user. We now use `fsnotify`'s `Event` type directly. This makes interoperability a lot easier. And it means we won't need to make any major changes here for future changes in `fsnotify`, like converting to `OsPath`.

We do some new clever stuff as well, like stopping the file watcher automatically at the end of the stream.

It is possible that we've gone too far. If there's anything you really miss from version 1, please mention it on the issue tracker!

## 1.1.1.0 -- 2020-05-27

- Export additional `EventPredicate`s.

## 1.1.0.0 -- 2020-05-27

- Use abstract newtype for `StopWatching`.
- Remove typed filepaths. Power-to-weight ratio was too low.

## 1.0.1.0 -- 2020-05-27

- George Thomas takes over as maintainer. Metadata changes.

## 1.0.0.1 -- 2019-12-09

- Widen bounds on `time` to support Windows build.

## 1.0.0.0 -- 2019-12-06

- First version. Released on an unsuspecting world.
