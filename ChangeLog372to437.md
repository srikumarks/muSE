# Introduction #

Describes changes between versions 372 and 437.

# Summary #

**Features**: Forward decaration using `define`, first class classes, `get` and `put` support arbitrary number of arguments, local import from a module, vectors can be used with `get` and `put`, OLE automation support.

**Experimental work**: `the` and `it` primitives for reducing need to name results.

**Bug fixes**:  Parsing, pretty printing and GC.

# Features #

  * The command line app now accepts filenames to load in the argument list. If you provide the --run switch, the repl will not be invoked at the end of loading all the files, thus effectively "running" the programs.

  * New apis - muse\_bindings\_stack\_pos(), muse\_bindings\_stack\_unwind(), muse\_nativefn\_context().

  * Enhanced define expression. Lets you "forward declare" functions and have the declared signature checked when the function is actually defined. Allows properly recursive function definitions without invoking dynamic scoping.

  * New "local" declaration for introducing local variables within function bodies.

  * Classes are made first class and can be exported from a module just like any other first class object. Classes can also be forward declared now.

  * The modulo operator is modified to do a proper mathematica modulo - the sign of the denominator is discarded and the numerator is converted into the range [0,denom).

  * "import" expression uses the 'scop' view so that it can be used within function bodies correctly.

  * New "isa?" function for checking whether a given class features in the inheritance hierarchy of a given object.

  * Modified signatures of get and put functions for the 'prop' view to accept an arbitrary number of arguments. The purpose is to allow nested get and put.

  * Added muse\_get() and muse\_put() exported functions to API. These perform recursive get and put operations on nested data structures.

  * Modified hashtable's 'prop' view implementation to support recursive get/put.

  * Added 'prop' view support to vectors. The keys are expected to be integers.

DESCRIPTION: By "recursive get/put", I mean the redefinition of get and put to
behave the following way -

`(get obj key)` -> Same as before, except that only the value will be returned
and not the cons pair.
```
(get obj key1 key2) -> (get (get obj key1) key2)
(get obj key1 key2 key3) -> (get (get (get obj key1) key2) key3)
.. and so on
```

Similarly for put -
```
(put obj key value) -> same as before
(put obj key1 key2 value) -> (put (get obj key1) key2 value)
(put obj key1 key2 key3 value) -> (put (get obj key1 key2) key3 value)
.. and so on.
```

**Note**: `get`, as opposed to `->`, does not search inheritance hierarchies.

  * Added OLE automation support to muSE.
You can now create OLE objects and invoke methods
via dynamic dispatch.

New functions -
```
   com-create
   com-release
   byref
```

COM objects support property getting and setting by supporting the
'prop' view - i.e. you can use get and put functions on COM objects.


# Experimental work #

See TheAndIt.

### OBSOLETE COMMENT ###
Introduced "it" (which is now a reserved symbol). In conditional
expressions such as if, when, unles, cond and case, the value of
the condition on which the branch occurs will be available in the
body of the conditional as "it". For example,

```
(if (find 3 '(1 2 3 4 5))
   (print it)
   (print "not found"))
```

will print

```
(3 4 5)
```

This is marked "experimental" since "it" may not be the right word
for it. Maybe "the-result" is better, but "it" seems the most concise.

"the" scope is now limited to within do blocks (and if, cond, case, etc.)
Also modified several algo functions to save their results in the recent list.

# Enhancements #

  * `define` could not be used in macro-generated expressions because it wasn't accounting for the possibility of being used with `apply`. The symbol argument can end up being a quick-quoted symbol and hence with a -ve cell value. Fixed this by `_quq()` on the symbol. Now you can write macro expressions that generate definitions (ex: enumerations).

  * When loading files specified in the command line, muSE now won't wrap the load operations into a try block. This is useful if the files create some objects for use in the REPL.

# Bug fixes #

  * To `muse_list_append` - some boundary cases.
  * Expressions like `e1`, `e2` were being recognized as numbers. Made such expressions be treated as symbols.
  * Objects that are marked to survive GC operations using `muse_mark()` are now released correctly when the environment is destroyed.
  * `muse_pwrite()` and `muse_pprint()` were doing a pretty-printer reset. This was causing pretty printing crashes in a few occasions due to mismatched indent/unindent. Fixed the crashes by removing the reset operations.

# Miscellaneous #

  * `return` and `break` are finally removed.
  * Made some functions non-static so that doxygen will include their documentation.
  * `muse_exception()` is now obsolete.