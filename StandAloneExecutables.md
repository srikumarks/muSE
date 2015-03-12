# Introduction #

Creating stand alone executables is one of the most important factors contributing to the usability of a language and software development system. The muSE interpreter has recently (since v102 in the [processes](http://muvee-symbolic-expressions.googlecode.com/svn/branches/processes) branch and since v149 on the [trunk](http://muvee-symbolic-expressions.googlecode.com/svn/trunk)) acquired that capability, in a very simple form however.

# Usage #

`muse --exec output.exe source1.scm source2.scm ...`

# Description #

Creates `output.exe` using `muse` as the base executable. It stores all the given source files in the given order in the executable itself, so that when the result is run, all the source files are `load`ed in the given order.

If any of the source files contains a function definition of the symbol `main`, the generated executable will invoke this function, supplying the command-line arguments as a list of strings as the argument.

For example, here is a main function that counts the number of arguments given -
```
(define main 
  (fn args
    (print "You gave" (length args) "arguments.")))
```

If no `main` function is defined, the executable starts the REPL after loading the attached source code. This is a convenient way to load a library of definitions by default.

# Advanced usage #

You can generate executables even using a generated executable! This is a way to augment an existing executable with new functionality. You simply use the same syntax as described in the Usage section with your generated executable.

# Implementation notes #

The technique used is brain dead simple.

  1. A copy of the executable is first made.
  1. The source files are appended to the executable in the given order.
  1. A 20-byte tag is appended to the end of the output to describe the source code section. The tag has the format "`;<10-digit-source-size> muSEexec`".

The interpreter first examines itself and checks for the ending sequence `muSEexec`. If that's present, it parses the 20-byte tail and extracts the total size of the source code that's been appended to the executable. (This size does not include the 20-byte tail.) It then opens itself for reading, seeks to the beginning of the source code section and loads the source code using the muSE API's `muse_load()` function.

Since the 20-byte tail begins with the scheme comment character, `muse_load()` will end up ignoring it.

In order to support executable expansion, the base executable is copied without its 20-byte tail (if present) and the source code size in the base executable is added to the total source size of all the new files to be included in the extended executable.

# Platform specific issues #

`GetModuleFileName()` is used in the Win32 version to get the full path to the executable that should be cloned.

On unices, the `argv[0]` is checked to see if its a valid file reference. It will be valid if you invoked muSE using the path to the executable - for example `/bin/muse` or even `./muse`. If this turned out to be invalid, it could be an alias or to be searched for using the path set in the environment. So in that case, muSE invokes the `which` command on the `argv[0]` to find the full path to the executable.