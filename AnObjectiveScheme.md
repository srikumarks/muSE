# Introduction #
The [objc branch](http://muvee-symbolic-expressions.googlecode.com/svn/branches/objc/), since v188, has an experimental bridge to the Objective-C runtime on MacOSX (10.4+) using a Smalltalk-like syntax that should look very familiar to Objective-C programmers. In fact, it is so similar that you can almost consider muSE as an _Objective Scheme_ interpreter. This page describes what you can and (since it is _experimental_) cannot do with the bridge.

[Download interpreter for MacOSX (Universal binary)](http://code.google.com/p/muvee-symbolic-expressions/downloads/list).

# Syntax #
Object notation uses Smalltalk and Objective-C style `[]` expressions with one of the following structures -
```
[target selector]
[target selectorPart1: value1 part2: value2 part3: value3 ...]
```

The first causes the _selector_ to be sent to the _target_ without any other arguments. The second sends the selector `selectorPart2:part2:part3:` to the _target_ with additional arguments _value1_, _value2_, etc.

The _target_ can either be a named Objective-C class such as `NSMutableDictionary` or a muSE variable that holds a wrapped Objective-C object.

It is important to note that due to the nature of Scheme syntax, there **must** be a space between the various terms. For example, Objective-C will accept the expression -
`[NSNumber numberWithInt:355]`
but muSE will end up treating the whole `numberWithInt:355` as a selector symbol. Therefore in muSE, you **have** to write -
`[NSNumber numberWithInt: 355]`
instead, with a space after the `:`. This is easy to get adjusted to.

That's all!

# Marshalling #

muSE provides a simple translation from basic muSE objects to corresponding Objective-C objects when sending messages.

## muSE to Objective-C ##
The following coersions are made from muSE objects to Objective-C types when they are used as arguments to messages -
  1. muSE integers can be used wherever an `NSNumber` or a C-native integer type such as `int` or `long` or `long long` is expected.
  1. muSE floats can be used wherever an `NSNumber` or a C-native `float` or `double` is expected.
  1. muSE text strings can be used wherever an `NSString` is expected.
  1. muSE vectors and lists are converted into `NSArray` objects.
  1. muSE hashtables are converted into `NSDictionary` objects.


## Objective-C to muSE ##
The following coersions are applied to message return values -
  1. C-native integers and floats are converted to muSE integers and floats.
  1. An Objectve-C object is wrapped into a thunk that can be passed as an object argument to subsequent messages.

Evaluating (or "forcing") the thunk using an expression like `(thunk)` will cause the object to be coerced into a corresponding muSE object. The following correspondences apply -
  1. `NSArray` -> vector
  1. `NSDictionary` -> hashtable
  1. `NSSet` -> list
  1. `NSNumber` -> integer or float as appropriate
  1. `NSString` -> text string

If the thunk of an unknown object is forced, it evaluates to a string description of itself - whatever is returned by sending the `description` message to the object.


## Example ##
The following code creates an `NSArray` with 3 objects, converts it back into a muSE vector and prints the result. Dumb example, yes, but serves to illustrate -
```
(let ((x [NSMutableArray array]))
  [x addObject: (/ 355 113)]
  [x addObject: 2.71828]
  [x addObject: "The mandatory HELLO WORLD!"]
  (print "Number of objects =" [x count])
  (print "NSArray as a vector =" (x)))
```

which will print -
```
Number of objects = 3
NSArray as a vector = {vector 3.14159292 2.71828 "The mandatory HELLO WORLD!"}
T
```

## Things returned via pointers ##

If an objc method takes a pointer to an `id` and returns an object in that memory location, you can call that method with a dummy object created in muSE using `@object` like this - `(define dummy (@object))`. Subsequently, passing `dummy` as the value for the pointer argument will cause the result object to be stored into `dummy`'s value.

C-native integer and float values returned via pointers can accept a normal muSE number object as an argument. The muSE number's contents will be modified with the returned value.

# Bundles #
At REPL startup, only the `Foundation` framework is linked in. If you want to use the classes in some other bundle, say `/System/Library/Frameworks/QTKit.framework`, you can load the bundle explicitly like so -
```
[[NSBundle bundleWithPath: "/System/Library/Frameworks/QTKit.framework"] load]
```

# muSE's own object system #

muSE has its own prototype based object system as documented in the [API docs](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__ObjectSystem.html). The square bracket notation equally applies to muSE's objects as well. The _target_ field accepts normal muSE objects and the method lookup is performed by concatenating all the selector parts. i.e. a method invocation such as
```
[x barkAtThief: "Bowow!" times: 5]
```
will lookup the function in `x` (or its inheritance hierarchy) using the key `barkAtThief:times:` and pass 3 arguments `x`, `"Bowow!"` and `5` to the function.

Since message passing can be seen as method invocation, a facility to make muSE objects look and behave like Objective-C objects from the other side of the bridge is also available - using the `@object` primitive.

If `x` is a normal muSE object, then `(@object x)` is a wrapper Objective-C object that will turn around and invoke methods of `x` whenever corresponding messages are sent to it.

Try this ([r711](https://code.google.com/p/muvee-symbolic-expressions/source/detail?r=711)) -
```
(let ((x (new)))
  (put x.value 100)
  (put x 'add:mul: (fn (self x y) (* (+ self.value x) y)))
  (print [x add: 3 mul: 2]))
```
It should print -
`206`

# Implementation #

`[]` expressions are macro expanded at read time into the canonical function call form. An expression such as -
```
[target selPart1: val1 part2: val2 part3: val3]
```
is transformed into -
```
(selPart1:part2:part3: target val1 val2 val3)
```
and the selector is immediately "compiled" into a native structure. The compiled selector then assumes responsibility for actually passing the message to the target, be it an Objective-C object or an Objective-C class or a muSE object.

The precise equivalent expression is -
```
({@selector selPart1:part2:part3:} target val1 val2 val3)
```
The `@selector` operator takes a selector symbol and returns its corresponding compiled selector structure.

## Garbage collection ##

  * A muSE cell which wraps a pure-Objective-C object (such as `NSArray`) is retained until the muSE environment has no references to the object and the garbage collector gets invoked.
  * A muSE cell which wraps a `MuseObject` - an Objective-C wrapper to a muSE symbolic object - is retained along with the wrapped muSE object until such a time when there are no references to both.
  * A muSE object wrapped as a `MuseObject` and is stored in a pure-Objective-C collection such as `NSArray` is not guaranteed to survive garbage collection unless the muSE environment holds references to a muSE cell wrapping the `NSArray` as well as the object thast it contains. For this reason, it is best to store muSE objects in muSE collections instead of using the Objective-C collections.

# Key-Value coding and the `MuseEnvironment` #

Since v209, there is new Objective-C class called `MuseEnvironment` whose purpose is to provide a hook into MacOSX's Interface Builder so that the model for a UI can be written in muSE using _Cocoa Bindings_.

To start, parse the `MuseEnvironment.h` interface into your Interface Builder project and instantiate a `MuseEnvironment` object. (Note that you can have **only one** `MuseEnvironment` object alive in an application - this is a restriction that might be removed in the future.) You now have access to a whole bunch of muSE functionality! All you have to do is to instantiate an `NSObjectController` and set its `content` to the `MuseEnvironment` instance.

The `MuseEnvironment` object, at class initialization time, prepares a muSE environment and automatically loads the application resource file "main.scm" if present. So this is where you put your muSE model code.

## Top level keys ##
All global symbols with defined values are available as keys for both `set` and `get` operations. All you do to tie, say, a text cell to a global muSE symbol defined like -
```
(define message "Hello world!")
```
is to bind the text cell to the `NSObjectController`'s `message` key (which you add to it).

If either your property value is derived from other properties, you need more control about the returned value or need to perform other operations when you change the value, you can can define `message` to be a _property function_. Such a function, when called with no arguments (like `(f)`), evaluates to the current value of the property and when called with a single value (like `(f newval)`) changes the property to the new value and will subsequently evaluate to the new value. In the context of an object method, a property function should be like `(fn (self) ...)` to compute the property's value and like `(fn (self newval) ...)` to change the property's value.

## Key paths ##
If a top level key is defined to an Objective-C wrapped muSE object (using `@object`), its properties become accessible to Cocoa Bindings as keys using the dot separated keypath notation specified by Cocoa Key-Value coding.

## An example ##
Here is the canonical "currency converter" model object though this isn't the simplest way to write this example -

```
(class Converter ()
    (amount 1.0)
    (rate 1.0)
    (result (fn (self) 
                (* (-> self 'amount) (-> self 'rate))))
    (@dependencies '((amount result) (rate result))))

(define converter (@object (new Converter ())))
```

The key `converter` now refers to the instance of the `Converter` class. Therefore you can refer to the `converter` object's properties as `converter.amount`, `converter.rate` and `converter.result` to bind GUI elements to these model elements.

Note that `converter.result` is dependent on the `amount` and `rate`. We indicate this using a dependencies list `@dependencies`. Each entry in the list specifies all the keys that change whenever the first element in the list changes. Therefore in the above example, we're telling the UI to update the `result` cell whenever `amount` and `rate` change.

A simpler implementation of this example is -
```
(define amount 1.0)
(define rate 1.0)
(define result (fn: () (* amount rate)))
(@dependencies '((amount result) (rate result)))
```

# TODO #
Lots!
  1. The marshalling is rather incomplete. Needs support for passing and returning C structures.
  1. Wrapping muSE objects as Objective-C objects is at present very incomplete though some basic cases work.