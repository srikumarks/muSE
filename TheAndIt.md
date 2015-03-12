# Introduction #

In programming, we often need to use identifiers - names - to refer to results of recent computations. It is a rather large mental overhead to keep inventing names for such temporary references, particularly since we're so used to using phrases such as "that issue I meant", "the pot on the table", "it is going to fall", etc. in natural language with our brains effortlessly filling out the context. I was wondering if we could do such contextual referencing in a programming language and here is my first attempt at it. This is my first (and so far only) attempt at designing a language feature inspired by a cognitive model.

I'm going to have a swell time describing `it` and `the` using normal English that's full of 'it's and 'the's, so the convention is `it` and `the` (in typewriter font) refer to the primitives while other uses are in normal font.

# Using `the` #

The behaviour of `the` exploits our ability to freely use a verb form as a noun when talking about what the verb did. For example, we are ok with referring to the result of "find a buried dinosaur" as "the find". We're also comfortable with mentally prefixing a noun with "calculate" when you look at `(first some-list)`. `the` therefore works by letting you flow freely between verbs and nouns in your head.

Here is a silly example you can do on the REPL (where it is particularly handy) -

```
> (find 3 '(4 2 3 1))
(3 1)
> (length (the find))
2
> (print (- (the length) 1) "more items after 3")
1 more items after 3
```

i.e. the `the` form has the structure `(the head-value)`. The recent few computations are searched for a matching head-value and the `the` form evaluates to the most recent value whose head matches the given head value. Note that the `head-value` is matched by value and not by name.

# Using `it` #

`it` is an identifier that refers to whatever the most recent `the` expression evaluated to. So you can continue the above toy REPL talk as -
```
> (if (<= it 2) 
      (print "I should've printed item in singular.") 
      (print "yeah, that's right."))
I should've printed item in singular.
```
In the above case, `it` is referring to the most recent `(the length)` expression.

# The nitty gritty stuff #

## Which results ##
Only the results of some primitive operations can be referred to using `the`. In particular, results of numerical operations cannot be referred to using `the`. It gets very tiring to determine what is the "most recent invocation of +" when reading code like `(the +)`. Hence the decision to exclude numerical operations. A thumb rule is that if a primitive is performing some substantial computation - `O(N)`, `O(log N)`, etc. - the result can be referred to using `the`.

## How many recent results ##
Only the most recent 8 results can be referred to. This is usually at the limit of our working memory when reading such code anyway, so it is sufficient. Further, the scoping rules make `the` quite easy to read because if you've wrapped some code into a function, then the one using the function doesn't see the intermediate results computed within the function and they don't count towards the 8. Now, that would be a riot if that were allowed, wouldn't it? So it is 8 results at whatever level you're working.

## Scoping ##
A `the` expression can access recent results within its block scope and those lexically above it. The number of recent items accessible is w.r.t this scoping rule - i.e. inaccessible results don't count in the 8. For example, see the following transaction -

```
> (if (find 3 '(4 2 3 1)) 
    (print (- (length (the find)) 1) 
           (if (= (the length) 2) "item" "items") 
           "after 3")
    (print "3 not found"))
1 item after 3
> (the find)
(3 1)
> (the length)
; Message box saying "No such recent computation by fn ... 
```

The same scoping rule applies to `it` as well.

## `define`d functions ##
The result of every `define`d function is available as a recent result and counts towards the 8. For example, let us define a function that symmetrizes a list by combining it with its own reverse.
```
> (define (symmetric-form a binop)
    (map (fn (p) (apply binop p)) 
         (transpose a (reverse a))))
> (symmetric-form '(1 2 4 8) +)
(9 6 6 9)
> (the symmetric-form)
(9 6 6 9)
> (define s symmetric-form)
> (the s)
(9 6 6 9)
> (eq? (the s) (the symmetric-form))
T
```

**Limitation**: You cannot currently close on a `the` form - capturing results from the environment of evaluation. For example, the following doesn't work yet (cont'd from example above) -
```
> (define (scale f) (map (fn (x) (* x f)) (the symmetric-form)))
> (scale 3)
; One would expect (27 18 18 27), but you'll get an error box as of v437.
; A `the` form within a function definition can only refer to results
; of computations in its own scope.
```
.. this feature will find its way into a future version, 'cos it is intuitive and handy.

**Update**: Although [r454](https://code.google.com/p/muvee-symbolic-expressions/source/detail?r=454) added support for this, it turned out to be too much of a hit on performance. As of [r580](https://code.google.com/p/muvee-symbolic-expressions/source/detail?r=580), recent scope capture in closures needs to be declared explicitly using [with-recent](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__CoreLanguage.html#gae40d359c2c4eba23134b5cc34d2ef2b5).

## Laziness ##
`the` and `it` work well with lazy computations too. Consider the lazy verbose enumeration function `enum/v` below, which prints out its head term every time it is evaluated -
```
> (define (enum/v m n)
    (if (< m n)
      (lcons (do (print "m =" m) m)
             (enum/v (+ m 1) n))
      ()))
> (enum/v 1 4)
(m = 1
1 m = 2
2 m = 3
3)           
```
Note that the list is evaluated as each element is printed out. Now try this -
```
> (do (enum/v 1 6)
      (print "squaring...")
      (map (fn (x) (* x x)) (the enum/v)))
squaring...
m = 1
(m = 2
1 m = 3
4 m = 4
9 m = 5
16 25)
```
The `(the enum/v)` expression refers to the as yet unevaluated list generated by the first `(enum/v 1 6)` expression. Only as the result of the map is being printed out does the `enum/v` generated list get fleshed out. That's why you see `squaring...` printed first before everything else.

# Notes #

  * I think `the` and `it` are amenable to compilation since they are lexically defined. For example, `it` refers to the "most recent `the` form" and that is a lexical feature. This property is not an accident. The ability to reference lexically is important for code to be readable and so I used it as a constraint in the design.

  * The introduction of `the` and `it` has been expensive on the interpreter. It is now nearly 8 times slower than without these primitives. Therefore I'm making this experimental until I figure out a way to eliminate the overhead.

# Conclusion #

I think this definition of `the` and `it` serves to reduce the need to define local variables significantly. In most languages, local variables lead to gross `let` nesting, particularly when dealing with imperative code. They also place a burden on the programmer to come up with names for temporary results too often, resulting in many resorting to `foo` and `bar` style nonsensical names.

The number of indirections that one has to mentally perform when reading code is also lower with `the` and `it`. if you name a result and use the name, you need to first look at the point where the name is defined and then at its definition (which is thankfully usually adjacent to it). When using `the` to refer to recent computations, you only need to search for the head term. When using `it`, you have to locate the immediately preceding `the` form to find out what `it` means.