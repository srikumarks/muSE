# Introduction #

Describes changes and fixes that have been in the works since [r372](https://code.google.com/p/muvee-symbolic-expressions/source/detail?r=372) of
muSE.

# Executive summary #

**Features**: New object system, revamped module system, generalized `get` and `put`, OLE automation support, `the` and `it`, alternative `tab-syntax`, miscellaneous utility functions such as `split`.

**Bug fixes**: Lots! Scalability fixes (have used muSE to process several 10s of MB of data.) Build and run on Linux.

**Other**: Ceased support for MacOSX/PPC binaries.

Primary reference is [the html documentation](http://muvee-symbolic-expressions.googlecode.com/svn/api/index.html).

# Features #
  * Better use of exception mechanism to raise conditions in builtin functions such as `open-file` and `load`.

  * In-built reading and writing of XML files (a simple useful subset of XML) and JSON objects.

  * Support for referring to recent computations using TheAndIt. They have the potential to dramatically reduce the need for transient local variables and make code more readable.

  * The command line app now accepts filenames to load in the argument list. If you provide the `--run` switch, the repl will not be invoked at the end of loading all the files, thus effectively "running" the programs. You can create StandAloneExecutables using the `--exe` switch.

  * Totally revamped prototype based ObjectSystem.

  * A simple, flexible first class ModuleSystem.

  * Support for OLE automation on Win32 using COM.

  * Generalized `get` and `put` operations which work on muSE objects, hashtables, vectors, modules and COM objects. Properties of objects can also be accessed using support for dot syntax such as `a.b.c`.

  * Better use of exception mechanism to raise conditions in builtin functions such as `open-file` and `load`.

  * In-built reading and writing of XML files (a simple useful subset of XML) and JSON objects.

  * Support for referring to recent computations using TheAndIt. They have the potential to dramatically reduce the need for transient local variables and make code more readable.

  * Support for running code under timeout constraints using `with-timeout-us`. The runtime will raise a resumable `'timeout` exception when the specified timeout occurs.

  * Support for an alternative `tab-syntax` for those allergic to parentheses. (My guess is you'll soon become allergic to tabs, but the choice is yours :)

# Misc #

  * `for-each`'s argument order has been changed. The first argument is the collection over which to iterate and the second argument is th iteration function. This is a deviation from the Scheme standard. The code just doesn't read correct if the argument order is the other way.

  * `mk-vector` and `mk-hashtable` now accept generator functions which can be used as a simple ``memoization'' technique.

  * `split` splits a string into substrings separated by the given separator set.
