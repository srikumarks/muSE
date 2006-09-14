muSE - muvee Symbolic Expressions

muSE is an interpreter for a Scheme dialect suitable 
for use as an embedded language in applications. 
It has a reasonably well documented C-API for embedding. 
For more info, see -

	http://code.google.com/p/muvee-scheme


== LICENSE ==

Please read the LICENSE.txt file for the license under 
which this software is being provided to you.


== BUILDING ==

Currently muSE builds on MacOSX and WIN32 platforms. 
It should build on most POSIX compatible unices as well, 
using GCC.

You simply need to load up all the C files in the src/ 
directory into your favourite IDE and hit build. You should 
get a working REPL.

A convenience script is provided for unices using GCC. Run 
"build-posix" from the command line and you should get the 
REPL executable in the bin/ directory.

If you wish to use muSE as a library, include all files 
except main.c in your project and set the project type to
"library" in your favourite IDE.

Over time, build scripts and project files for various IDEs 
will be added.
