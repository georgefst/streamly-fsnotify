# ``streamly-fsnotify``

## What's the deal with this library?

[``streamly``][1] is an undoubtedly awesome library - [fast][2], flexible, and
well-documented. File system watching is a natural fit for a streaming library,
and this is exactly what ``streamly-notify`` provides you:

```haskell
import Streamly.FSNotify

import qualified Streamly.Prelude as SP

```

## That seems pretty cool! What kind of features can I expect?

* Cross-platform - works anywhere both ``streamly`` and ``fsnotify`` do.
* Efficient (event-driven, so won't shred your CPU or load your RAM).
* Able to do one-level and recursive watching.
* Extensive set of filtering predicates, so you don't have to see events you
  don't care about!
* Type safe (both itself, and in the components it uses).

## Sounds good? Can I use it?

## License

This library is under the GNU General Public License, version 3 or later (SPDX
code ``GPL-3.0-or-later``). For more details, see the ``LICENSE.md`` file.

[1]: http://hackage.haskell.org/package/streamly
[2]: https://github.com/composewell/streaming-benchmarks
