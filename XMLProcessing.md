# Introduction #

These days, a lot of data exchanged between computer programs gets done using an XML structure. Be it SOAP, XML/RPC, Jabber, whatever, there is no escaping having to deal with simple XML data structures, even though they can all be mapped perfectly well to equivalent s-expressions.

Enter [read-xml](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__PortIO.html#g43e20503d130213a336ab78e7d64f28d) - a function that does exactly that - read a single XML node from any port and presents it as an equivalent s-expression.

# An example #

The xml expression below gets transformed to an s-expression as shown -
```
<article id='Hudak92'>
    <author> P. R. Hudak </author>
    <author> J. H. Fasel </author>
    <title> A Gentle Introduction To Haskell </title>
    <journalref name="ACM SIGPLAN Notices" volume="27" number="5" pages="1--53"/>
    <date year="1992"/>
    <keyword>functional</keyword>
    <keyword>tutorial</keyword>    
</article>
```
becomes
```
(article ((id . "Hudak92"))
    (author () "P. R. Hudak")
    (author () "J. H. Fasel")
    (title () "A Gentle Introduction To Haskell")
    (journalref ((name . "ACM SIGPLAN Notices")
                 (volume . "27")
                 (number . "5")
                 (pages . "1--53")))
    (date ((year . "1992")))
    (keyword () "functional")
    (keyword () "tutorial"))
```

Though in its current incarnation it is only useful for data represented as XML as opposed to marked up text, it can handle a broad enough subset to be useful in a variety of contexts.

[read-xml](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__PortIO.html#g43e20503d130213a336ab78e7d64f28d) and [write-xml](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__PortIO.html#g1e609c18047e7fcd3bfb12bf012c5145) are duals.

# Querying for data #

Simply having XML data represented as an s-expression is rather useless if you have to write complicated programs to traverse the tree to extract data useful to your case at hand. It is good to have a collection of functions that you can compose in order to express the portion of the xml node that you're interested in.

[xml.scm](http://muvee-symbolic-expressions.googlecode.com/svn/trunk/examples/xml.scm) defines an expressive collection of predicates and combinators that for this purpose. The basic design of the predicates and combinators is described below. The `xml.scm` file defines the `XML` module containing the operator bindings. To bring in the bindings exported by the XML module into the current work scope, you can write -
```
(import (load "xml.scm"))
```

## Data extractors ##

The functions `tag=`, `attr=` and `body=` are used to extract the tag name, the value of an attribute and the body of a node respectively.

## Predicates ##

A predicate is a function that takes an XML node (as an s-expression) and evaluates to a list of XML nodes that satisfy the condition of the predicate.

```
fn (node) -> list-of-nodes
```

For example, the expression `(:tag article)` evaluates to a predicate that evaluates to `(node)` if the tag of the node is the symbol `article` and to `()` if it isn't. The expression `(:attr year (= year "1992"))` evaluates to a predicate that evaluates to `(node)` if it contains an attribute `year` with the value `1992`.

The `list-of-nodes` monadic representation was chosen because it lends itself to composition using `map` and `join` functions.

## Combinators ##

These are functions that take other predicates and create a new predicate.

For example, the `:and` combinator takes a number of predicates and evaluates to a predicate that will succeed with those nodes that satisfy all of the given predicates. The expression
```
(:and (:tag journalref) (:attr name (= name "ACM SIGPLAN Notices")))
```
is a predicate that will match the `journalref` tag as shown above.

Here are some other combinators -
  * `(:or ..preds..)` succeeds with a list of nodes each of which satisfy at least one of the argument predicates.
  * `(:not pred)` succeeds with a list of nodes that don't satisfy the given predicate.
  * `(:child pred)` succeeds with the list of child nodes that satisfy the given predicate.
  * `(:descendant pred)` succeeds with the list of descendant nodes - including the root node itself - that satisfy the given predicate.
  * `(:select pred1 pred2 ..)` the results of `pred1` are refined by `pred2` which are further refined by the remaining predicates.
  * `(:path pred1 pred2 ... predN)` matches the deepest node `predN` such that `pred1` is `pred2`'s parent, `pred2` is `pred3`'s parent, and so on.

### Examples ###
Here is a new predicate that matches a node if the first element of its body is the string `"functional"`. It shows how you can define your own predicates.
```
(define kw-functional 
    (fn (node) 
        (if (= (first (body= node)) "functional") 
            (list node)
            ())))
```

A query for all articles after 1990 that match the keyword "functional".
```
(define functional-articles-after-1990
    (:and (:tag article) 
          (:child (:and (:tag date) (:attr year (> (number year) 1990))))
          (:child (:and (:tag keyword) kw-functional))))
```

A query to extract all the "keyword" tags.
```
(define all-keywords (:descendant (:tag keyword)))
```

A query to extract all articles between 1990 and 1995.
```
(define articles-between-1990-1995
    (:and (:tag article)
          (:child (:and (:tag date) 
                        (:attr year (and (>= (number year) 1990)
                                         (<= (number year) 1995)))))))
```