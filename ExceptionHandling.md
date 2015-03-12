# Introduction #

During modular development of applications and libraries, you often come across cases that you cannot handle and must delegate decisions to the caller. muSE, like most other languages, provides support for managing such conditions referred to here as _exceptions_.

This document is written with reference to **version 343** of muSE.

# Comparison with other languages #

Languages such as Java and C++ provide easy to use ways to raise and detect exceptions. However, their `try`/`catch` constructs don't provide adequate ways to recover and continue when a higher level piece of code determines that an exceptional condition does not really warrant termination of the task. This is due to the fact that the context in which the exception occurred is destroyed by the time the code reaches a "handler".

Languages such as Common Lisp, by not destroying the execution context before invoking exception handling code, are highly flexible in dealing with exceptions.

muSE borrows the _suspension of context_ idea from Common Lisp and tries to make it available in a form that's as easy to use as Java/C++ by borrowing terminology from them. Like Common Lisp, it goes beyond raising and detecting exceptions by enabling a coder to design a function that adapts to the context in which it is invoked.

# `raise`: Raising exceptions #

In muSE, expressions evaluate to values. When the value of an expression you write cannot be determined because a condition for validity wasn't satisfied, you can specify an exception as the value instead. For example, consider a function `H(x,y)` that computes `(x+y)/(x-y)`.
```
(define (H x y) (/ (+ x y) (- x y)))
```

The value of `H` is undefined if `x` happens to be equal to `y`. We can indicate this situation by using a `raise` expression as the value when the condition occurs, as shown below -
```
(define (H x y)
  (/ (+ x y)
     (if (= x y)
         (raise 'DivideByZeroDiff x y)
         (- x y))))
```

The `raise` construct can take as many arguments as deemed necessary by the designer of the function. Since `H` flags this condition using `raise` its calling context can detect and handle this special case.

# `try`: Detecting exceptions #

To be able to detect any exceptional conditions that might be raised by an expression `e`, you need to place it as the first expression of a `try` construct, like this -
```
(try e ...)
```

The expressions following `e` specify _handlers_ for the conditions that might be found when evaluating `e`. Handlers are ordinary muSE functions and are tried in sequence. A handler takes effect when its argument pattern matches against the value pattern of the `raise` expression that raised the condition. For example, here's how we'd handle the `DivideByZeroDiff` exception raised by our humble `H` function, in a function `G(x,y,a,b)` which calculates `H(x,y)/H(a,b)` -
```
(define (G x y z w)
  (try (/ (H x y) (H z w))
       (fn (ex 'DivideByZeroDiff x y)
           ...)))
```
(Ignore the `ex` argument for the moment. We'll come to it soon.)

Note that there are two evaluations of `H` in `G`. The handler we gave will be invoked when either evaluation raises an exception.

# Resuming exceptions #

Let's continue with the example we've been working with above.

Say according to requirements faced by `G` we can allow `H` to take on `(x+y)/MINDIFF` whenever `x=y`. We can specify this in our handler by declaring that the `raise` expression that invoked the handler should take on the value `MINDIFF`. We say this as follows -
```
(define (G x y z w)
  (try (/ (H x y) (H z w))
       (fn (ex 'DivideByZeroDiff x y)
           (ex MINDIFF))))
```

The first argument to an exception handler is called a _resume continuation_. Its sole purpose is to be able to state that the exception is not a problem serious enough to termnate computation and that the `raise` expression may safely be replaced by a given value.

Note that by doing so, we've taken an incomplete `H` and created a complete function `G` with no exceptions. An alternative way to make `G` complete is to use a special value `UNDEFINED` (or something like that) as the result of `G`. We can do that as follows -
```
(define (G x y z w)
  (try (/ (H x y) (H z w))
       (fn (ex 'DivideByZeroDiff x y)
           UNDEFINED)))
```
The value of a `try` expression is either the value of the expression it controls when no exceptions occurred, or the value of any of its handlers that handled an exception. Therefore just using `UNDEFINED` as the result of our handler caused `G`'s result value too be `UNDEFINED` when either `x=y` or `z=w`.

> Note that if `x=y`, evaluation of `(H z w)` has been _short-circuited_ by our exception handler.

# `retry`: Alternative execution paths #

What if `G` wanted to say "whenever `H` raises an exception, simply use a fixed value `HNORM` as its value instead of continuing with its computation. As defined, our `H` does not let `G` specify that easily. Let's modify `H` to be even more flexible in this matter -
```
(define (H x y)
  (try
    (/ (+ x y)
       (if (= x y)
           (raise 'DivideByZeroDiff x y)
           (- x y))))
    {fn ('ForceResult r) r}))
```

What we've made `H` declare by the above modification is that it allows its result to be replaced with an arbitrary value decided by a caller function whenever the `DivideByZeroDiff` exception is encountered. So we can now write `G` as -
```
(define (G x y z w)
  (try (/ (H x y) (H z w))
       (fn (ex 'DivideByZeroDiff x y)
           (retry 'ForceResult HNORM))))
```
`G` now causes `H` (either invocation) to take the `ForceResult` execution path by specifying a `retry` expression whose argument pattern matches that required by the alternative code path allowed by `H`.

> Note that a function should declare all available alternative code paths in its documentation in order for callers to take advantage of them.

# `finally`: Resource cleanup #

The example we've used so far is a mathematical fuction, so `try`, `raise` and `retry` are adequate facilities. A large number of exceptional conditions, however, are encountered when dealing with i/o. In such cases, we additionally need to perform some cleanup action, such as closing a file, that must be executed before a function can return.
Consider an (albeit contrived) example, where we need to write a function `lookup-db` that looks up the value corresponding to a given symbol as specified in a file and returns it. It is expected to always use the latest version of the file and cannot cache its results. From the point of view the user of `lookup-db`, it should be practically indistinguishable from a mutable hashtable.
What does `lookup-db` need to do? It needs to first open the database file (which in our case will contain a single association list), read a single association list from it, lookup the key in the assoc list, close the file and return the associated value.
There are a number of things that can go wrong here -
  1. The file may not exist, so `open-file` may fail.
  1. The file may be corrupt and reading it may not give us a list.
  1. The given key may not be found in the assoc list.

So lets write a `lookup-db` that flags all these conditions using exceptions -
```
(define (lookup-db db key)
  ; Open the database file.
  (define dbfile 
    (let ((f (open-file db 'for-reading)))
      (if (eof? f)
          (raise 'DatabaseFileError db)
          f)))
          
  ; Read the association list.
  (define dbtable
    (let ((kvs (read dbfile)))
      (case kvs
        (((k1 . v1) . etc) kvs)
        (_ (raise 'DatabaseInvalid db dbfile)))))
  
  ; Perform the lookup
  (define kv
    (case (assoc dbtable key)
      (() (raise 'DatabaseKeyNotFound key dbtable dbfile))
      (_ _)))

  ; Close the file
  (close dbfile)
    
  ; The value is the value part of kv.
  (rest kv))
```

Our intention here is that the `(close dbfile)` expression should always be called before `lookup-db` completes. If the caller resumes all of the exceptions that might occur before we get to `(close dbfile)`, we're safe. If the caller decides to use a default value when any one exception occurs, `(close dbfile)` will never be evaluated and we'll be left with an open file deescriptor. Its ultimately not a problem because the garbage collector will close the file, but system resources are valuable enough to release as soon as we're done with them, so we should close the file before `lookup-db` completes.

Here's how we do that -
```
(define (lookup-db db key)
  (try
   (do
     ; Open the database file.
     (define dbfile 
       (let ((f (open-file db 'for-reading)))
         (if (eof? f)
             (raise 'DatabaseFileError db)
             f)))
     
     ; Postpone the closing to whenever we 
     ; exit the try block.
     (finally (close dbfile))
     
     ; Read the association list.
     (define dbtable
       (let ((kvs (read dbfile)))
         (case kvs
           (((k1 . v1) . etc) kvs)
           (_ (raise 'DatabaseInvalid db dbfile)))))
     
     ; Perform the lookup
     (define kv
       (case (assoc dbtable key)
         (() (raise 'DatabaseKeyNotFound key dbtable dbfile))
         (_ _)))
     
     ; The value is the value part of kv.
     (rest kv))))
```

Note that we declare that the `(close dbfile)` expression **must** be evaluated before leaving the enclosing `try` block. This simple mechanism satisfies the following cases -
  1. The database file will not be closed when inside any exception handler specified in the caller. Therefore a caller can decide that the next expression in the database file should be tried if the first one didn't satisfy some condition.
  1. If a caller handles `'DatabaseFileError` and resumes the exception by opening a "fallback database" and passing the file handle to the resume continuation, the `finally` will correctly close the fallback database.
  1. If a caller is written like this -
```
    (try (lookup-db "database.scm" 'Platform)
         (fn (ex 'DatabaseKeyNotFound key dbtable dbfile)
            "MacOSX"))
```
> > meaning a default value is used if the key could not be found, thereby escaping the caller's own `try` block. `dbfile` will still end up being closed before the caller's `try` block completes.