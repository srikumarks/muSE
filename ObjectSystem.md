# Introduction #

Polymorphic method dispatch is one of the most important concepts introduced into the world of programming under the banner of "object oriented programming". muSE provides a simple object system for the express purpose of borrowing and expanding on behaviour by composing networks of objects.

# Basics #

A muSE "object" is an entity with attached properties and methods. Objects are created using [new](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__ObjectSystem.html#g2d9edcd01eedd18c17f6859c239987a6). There is no separate notion of ``class''. Objects simply inherit from other objects.

# Getting and setting properties #

The `get` and `put` operators are used to get and set properties of an object. Here is a sample transcript on the muSE REPL -
```
> (define Size (new))          ; A new garbage collectable object - an anonymous symbol.
{object ()}
> (put Size 'width 320)         ; Sets the 'width property to 320
320
> (put Size 'height 240)        ; Sets the 'height property to 240
240
> (* (get Size 'width) (get Size 'height))   ; Computes the area
76800
> (* Size.width Size.height) ; Equivalent to the line above.
76800
```

# Sending messages #

An object can have a function as a property's value, in which can you can use the function to implement a method to be performed by the object when you send it a corresponding message. Message sending is done by using the object itself in the function position and passing the method selector as the first argument. To continue with the example above -
```
> (put Size.area (fn (self) (* self.width self.height)))
     ; Defines an 'area method. Note that the first argument is always the object itself.
     
> (Size 'area)              ; Ask the size object to compute its own area.
76800
```
Any additional arguments passed to the object after the selector (such as `'area`) are passed on to the method after the first `self` argument.

# Supers #

Any object can borrow properties and methods from any other object through its ``supers'' list. You can get and set an object's supers list using [supers](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__ObjectSystem.html#g3cf21488c96a0cd1b0c0a0f73f47d068).

Here is a `screen` object which borrows the `'area` method from the `size` object described above -
```
> (define screen (new Size))
'{object ({object () 'area (fn (self)) 'height '240 'width '320})}
> (put screen.width 1024)           ; Set width to 1024
1024
> (put screen.height 768)           ; Set height to 768
768
> (screen 'area)                 ; Compute its area
786432
> (supers screen)                ; Get its super list
'({object () 'area (fn (self)) 'height '768 'width '1024})
```

# That's it! Really! #

# Search algorithm #

Here's how `get` searches for a property -
  1. First look in the given object's plist for the property. If found use that.
  1. If not found, check the object's supers list in the given order, recursively. If not return `()`.

The method invocation syntax is really sugar for a particular way to use the `get` operator -
```
(obj 'message arg1 arg2 ...)
```
is precisely equivalent to -
```
((get obj 'message) obj arg1 arg2 ...)
```

# Implications #

## A class is really just an object. ##

There is no separate notion of a "class". Any object with a bunch of properties and methods can be treated like a class from which other objects can borrow functionality and defaults. This lets us create ad hoc hierarchies of objects with inherited behaviours and properties.

## There is no distinction between class and object methods. ##

A method is a method. If you send a message to an object (an "instance"), the method gets performed on the object. If you send it to its class, it gets performed on the class - i.e. the parent object. To maintain a distinction between class and instance methods is completely up to the programmer.

## There are no constructors and destructors. ##

There is no need for them. A constructor is simply an initialization method, so no special provision is required. The garbage collector takes care of releasing the object, so no destruction methods are needed. Timed invocations of methods can be handled using the usual `try` and `finally` constructs.

## New functionality can be incorporated as late as necessary. ##

The super tree of an object can be edited at any time, even by a method. This lets us add functionality to a class of objects by loading definitions from a file after many objects have already been created. The objects automatically inherit all the new methods.

This is similar to the use of "categories" in Objective-C.

## Duck typing ##

Two objects could be said to be of the same type if they both have the same set of properties and methods as defined by the `->` and `<-` operators. An object that looks like another and quacks like another can be substitute for the other - something that Ruby folks affectionately call _Duck typing_.

## Reflection ##

An object's properties, methods and supers can be examined by code since everything is public. There is no information hiding, although there is encapsulation. The system, therefore, is fully reflective.

# See also #
  * AnObjectiveScheme
  * [Details and syntactic sugar for the object system described above.](http://muvee-symbolic-expressions.googlecode.com/svn/api/group__ObjectSystem.html)