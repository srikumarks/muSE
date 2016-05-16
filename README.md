muSE - short for 'muvee Symbolic Expressions' - is a Scheme dialect intended for use as an embedded language. It has some unique language features such as first class macros, simple general read-time evaluation syntax, vectors and hashtables that can be used as functions, and extensive use of pattern matching bind.

**Highlight**: muSE is used as an embedded language to specify the "styles" in [muvee Reveal](http://www.muvee.com/en/products/reveal/), launched on 11 June 2008.

## Language and library features ##
  * Provides lexically scoped closures as well as dynamically scoped blocks.
  * Expressive reader macro system where macros are first class citizens (i.e. can be passed around as arguments to functions, assigned to variables, etc.)
  * Uniform use of pattern matching for variable binding.
  * Generic functions ([doc](https://cdn.rawgit.com/srikumarks/muSE/master/api/group__GenericFns.html))
  * Many polymorphic primitives - including map, filter, reduce, get, put, and some others.
  * An extremely easy to use Objective-C bridge (only on MacOSX).
  * A simple module system.
  * Flexible exception handling
    * Resumable exceptions
    * Exception handler dispatch using pattern matching bind.
    * Simple cleanup mechanism
  * Reduce the need for local variables using TheAndIt.
  * Built-in support for [JSON](https://cdn.rawgit.com/srikumarks/muSE/master/api/group__PortIO.html#gad279620db34a8d1055531730840159ea).
  * Built-in support for reading/writing [a subset of XML](https://cdn.rawgit.com/srikumarks/muSE/master/api/group__PortIO.html#ga1e609c18047e7fcd3bfb12bf012c5145) .

## Integration features ##

  * C-based, fairly well documented, simple embedding API. Can call as well as be called from C/C++ code.
  * Compact - Its easy to strip down the feature set to whatever subset of the core language you need.
  * Extensible notion of objects to add new kinds of things to muSE in native code. Vectors, hashtables and ports (for I/O) are provided using this mechanism.
  * Ability to add C/C++ based native functionality in the form of plugin DLLs.
  * Ability to create StandAloneExecutables
  * Supports multiple independent environments
  * Liberal license terms - can use in commercial applications without publishing source code.

## License summary ##

The license agreement has been changed (13Nov2008) to "New BSD" with the requirement that contributions be made on the same terms.

## Copyright ##
Copyright (c) 2006, Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd.
