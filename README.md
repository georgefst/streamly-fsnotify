This library provides a higher-level, stream-based API for [`fsnotify`](https://hackage.haskell.org/package/fsnotify). It should work anywhere both `streamly` and `fsnotify` do. Note that there's no need for your project to register a dependency on `fsnotify` directly, since we re-export the two types you'll need. You will however need to depend on `streamly` in order to get anything done.
