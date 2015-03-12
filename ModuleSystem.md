# Introduction #

When programs get bigger, you need some way to isolate groups of functions into namespaces. muSE has a simple module system for this purpose.

A module is a muSE object that acts as a namespace for a set of ``exported'' symbols. The entities exported by a module are accessible using muSE's dot syntax. Any kind of object can be exported by a module - primitives such as numbers, strings, collections (vectors, hashtables), objects, functions and macros and even other (sub) modules.

# Specifying a module #

If you are not defining or exporting macros, modules can be specified as self-contained s-expressions like this -
```
(module ModName (export1 export2 ...)
  (define export1 ...)
  (define export2 ...)
  ...
)
```

If you're defining, using and exporting macros, then you need to make use of the reader **during** module definition and therefore you need to place your module in a separate file and use the following module definition header -
```
(module ModName (export1 export2 ...))
..module body...
```

In that case, the module's body is taken to extend to the end of the file. If a file contains a `(module ..)` declaration at the top, then a `(load ..)` of the file will result in the module object as the value.

Any `define` expressions within a module body that don't define exported symbols are considered to define private entities.

# Using a module's exported symbols #

Outside the scope of the module definition block, you can refer to the exported symbols as `ModName.export1`, `ModName.export2` etc. Those are the full names of the exported symbols.

**Note**: Unfortunately, exported macros can't be used that simply, because they are invoked at read time. You have two options to use an exported macro -
  1. Name the exported macro using `(define mymacro ModName.exportedMacro)` and then use `mymacro` in the rest of the code.
  1. Use brace expansion - `{ModName.exportedMacro ...args...}`

If you don't want to use the long form names of a module's symbols, you can use the `import` function to bring a module's definitions into the local scope like this -
```
(import ModName ...)
```
`import` accepts more than one module name. After the above `import` expression, the symbols `export1`, `export2`, etc. will be as defined within the module `ModName`.

`import` can be used in the body of an `(fn args ..)` expression as well. Though the effect of such an import statement is the same as for the use within a module as above, its appearance is different - the import is performed at closure creation time and the import expression itself disappears from the body of the created closure so as not to pose any execution overhead.

# Forward references #
Within a module definition, you can refer to defined symbols after their definition using their short form names. If you need to refer to an exported symbol before it has been defined using `define`, you need to use the long form name. For example -
```
(module Test (even odd)

  (define (even n)
    (case n
      (0 T)
      (_ (Test.odd (- n 1)))))

  (define (odd n)
    (case n
      (0 ())
      (_ (even (- n 1)))))
)
```

Notice that the definition of `Test.even` refers to `Test.odd` using the full name and the definition of `Test.odd` refers to `Test.even` using the short name `even`.

An alternative is to forward declare the mutual recursion as follows -

```
(module Test (even odd)

  (define (even n)) ; Forward declaration

  (define (odd n)
    (case n
      (0 ())
      (_ (even (- n 1)))))

  (define (even n)
    (case n
      (0 T)
      (_ (odd (- n 1)))))

)
```

If you're defining private mutually recursive functions, you might want to consider declaring them `local` first before the forward declarations, using -
```
  (local even odd ...)
```
- so that your local definitions don't borrow anything from the context outside the module body.  For example -
```
(module Test (classify)

  (local even odd) ; module-private stuff.
  
  (define (even n)) ; Forward declaration

  (define (odd n) ..) ; Defn of odd
  (define (even n) ...) ; Defn of even

  (define (classify n) 
    (cond ((even n) 'even) 
          ((odd n) 'odd)))
)
```


# Nested modules #
A module can export any kind of symbol, even other modules, so you can nest modules. The dot notation and `get` and `put` can support nested module export. For example -
```
(module Num (Test)
  (module Test (even odd) ...)
)
```
lets you access the `even` and `odd` functions as `Num.Test.even` and `Num.Test.odd` respectively.

# Examples #
Lets write a function that uses the `even` and `odd` functions defined above that maps numbers to the symbols `'even` or `'odd` accordingly.
```
(define (classify n)
  (cond
    ((Test.even n) 'even)
    ((Test.odd n) 'odd)))
```
So
```
> (classify 4)
even
> (classify 5)
odd
```

Suppose we don't want to write `Test.even` and `Test.odd` all over the place. We can do one of two things -
```
(import Test)

(define (classify n)
  (cond
    ((even n) 'even)
    ((odd n) 'odd)))
```

Of course, this introduces the `even` and `odd` symbols for all code loaded after `classify`. This may not be desirable in many circumstances, in which case we can do the following -
```
(define (classify n)
  (Test
    (cond
      ((even n) 'even)
      ((odd n) 'odd))))
```
Placing the `cond` expression within a `Test` block makes the `even` and `odd` symbols available without full qualification.