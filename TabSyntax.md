

# Introduction #

The common Scheme syntax using parentheses is very easy to get started on because there are so few exceptions to any rule and there are very few rules to begin with. If you use a special editor such as [DrScheme](http://plt-scheme.org), the syntax hardly ever gets in the way as most beginners might think. However, for some applications where Scheme code might need to be read and edited on a plain text editor such as Notepad, the parentheses do start getting in the way since you get no help to figure out matching parentheses. muSE provides a 2D syntax called `tab-syntax` similar to Python and Haskell.

The goals of the `tab-syntax` are -
  * To remove the need for parentheses under the most common circumstances,
  * To clearly separate lists and function calls using different notations for them,
  * To provide some support for infix notation to handle common math expressions.

You start using the `tab-syntax` in a source file by placing the line -
```
{tab-syntax}
```
The `tab-syntax` function switches the syntax on the port currently being read, which is the source file in most cases. You can also do the same on the REPL.

The following sections assume you are in `tab-syntax` mode.

# Comments #

The comment character remains `;` (the semi-colon) in `tab-syntax` mode too.

# Grouping #

`a`, `(a)`, `((a))`, `(((((a)))))` all mean the same thing - `a`. The parentheses serve to ''group'' and therefore a group of a group is considered to be still a group.

If the group's body contains more than one item such as in `(a b c)` it is taken to mean a function call - the function `a` is applied to the arguments `b` and `c`. In this case, we refer to `a` as the ''head term''.

If `(a b c)` occurs on a line all by itself, you can omit both parentheses. Therefore
```
(print "hello world" 42)
```
in Scheme syntax becomes
```
print "hello world" 42
```
in `tab-syntax`.

Of course, you're free to keep the parentheses in `tab-syntax` mode as well. Just that `(a b c)` and `(((a b c)))` will mean the same thing.

# Lists #

A list is notated as `[a,b,c,...]` - i.e. square brackets with items separated by commas. A list such as `[a b, c d, e f g]` will mean (in scheme-syntax) `((a b) (c d) (e f g))`

That is, multiple terms between commas get grouped and interpreted as function calls.

# Indentation and grouping #

```
hello world
   how are you doing?
      world: "hot!"
   darn!
```
is parsed as the scheme-syntax list -
```
(hello world (how are you doing? (world: "hot!")) darn!)
```

# Operators #

Any valid scheme symbol without any alphanumeric characters in its
textual form is seen as an operator.
  * Operators behave like normal functions when used as a ''head term'', so `(+ 1 2)` is still valid in `tab-syntax` mode.
  * In `tab-syntax` mode, `(a OPERATOR b...)` is interpreted as `(a OPERATOR (b...))`. This means `2 * 3 + 4` will be interpreted as `2 * (3 + 4)` and will therefore evaluate to 14. In other words, operator calculations are made right to left.

There are two special operators that are not functions and are
unwrapped at read time. These are the cons operator `:` and the
nesting operator `::`.

`a : b...` is the tab-syntax expression for a list with head term `a` and tail term `b...`. The first term of the `b...` expression is a head term, just like for any other operator. One use of the `:` operator is to express that a function be applied to a long list of arguments split across multiple lines. For example -

```
sum : [1, 2, 3,
       4, 5, 6,
       7, 8, 9]
```

which in Scheme-syntax means
```
(sum 1 2 3 4 5 6 7 8 9)
```

`a :: b...` is simply converted to `(a (b...))`. This is very useful syntactic sugar for `let`, `cond` and `case` expressions.

# Fringe cases #

Since `a`, `(a)` and `((((a))))` all mean the same, we need some way to say "call this function with no arguments". We use the ''empty group'' for this purpose, so `a ()` has the scheme-syntax equivalent `(a)`.

It is interesting to note that the need for this hack is simply because Scheme and muSE are not pure functional programming languages. In a pure functional language, a no-argument function is indistinguishable from a constant. That fact is reflected in Haskell's notation in which you cannot express a call to a no-argument function.

# Examples #

## Dot product of vectors ##
Here is a function for computing the dot product of two vectors expressed as lists -

In Scheme syntax -
```
(define (dot vec1 vec2 result)
   (if vec1 
       (dot (rest vec1) (rest vec2) (+ result (* (first vec1) (first vec2))))
       result))
```

In `tab-syntax` -
```
define (dot vec1 vec2 result)
  if vec1
    dot (rest vec1) (rest vec2) (result + (first vec1) * (first vec2))
    result
```


## Pairing up the elements of a list ##

The function `pair-up` takes a list of the form `(1 2 3 4 5 6)` and turns it into `((1 2) (3 4) (5 6))`.

In Scheme syntax -
```
(define (pair-up ls result)
  (case ls
    (()          (reverse! result))
    ((x)         (raise 'OddListLength))
    ((x y . xys) (pair-up xys (cons (list x y) result)))))
```

In `tab-syntax` mode -
```
define (pair-up ls result)
  case ls
    []             :: reverse! result
    [x]            :: raise 'OddListLength
    (x : y : xys)  :: pair-up xys (cons (list x y) result)
```
