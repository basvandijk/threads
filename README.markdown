This package provides functions to fork threads and wait for their
result, whether it's an exception or a normal value.

Besides waiting for the termination of a single thread this packages
also provides functions to wait for a group of threads to terminate.

This package is similar to the [threadmanager], [async] and [spawn]
packages. The advantages of this package are:

* Simpler API.

* More efficient in both space and time.

* No space-leak when forking a large number of threads.

* Correct handling of asynchronous exceptions.

* GHC specific functionality like [forkOnIO] and [forkIOUnmasked].

[threadmanager]:  http://hackage.haskell.org/package/threadmanager
[async]:          http://hackage.haskell.org/package/async
[spawn]:          http://hackage.haskell.org/package/spawn
[forkOnIO]:       http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Conc-Sync.html#v:forkOnIO
[forkIOUnmasked]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Conc-Sync.html#v:forkOnIOUnmasked
