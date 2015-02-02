---
date:         2014-09-13
type:         post
title:        First impressions of Swift
description:  Apple wants to replace Objective-C and could do a lot worse.
author:       Jonas Westerlund
tags:         apple ios mac objective-c programming swift 
---

With the introduction of [iOS 8](https://en.wikipedia.org/wiki/IOS_8) and [Mac OS X 10.10](https://en.wikipedia.org/wiki/OS_X_Yosemite), Apple is finally providing an alternative to the aging Objective-C.
It's called [Swift](https://en.wikipedia.org/wiki/Swift_(programming_language)), it's designed by [Chris Lattner](http://nondot.org/sabre/) of [LLVM](http://llvm.org) fame and I believe it's a major improvement over Objective-C.
I've summarized my first impressions in this post.

## Parametric polymorphism
Objective-C lacks any form of polymorphism other than subtyping, making you resort to `void *`{.c} and types such as `NSArray *`{.c} which do not express what the array contains.
In contrast, Swift supports [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) and allows you to say `Array<String>`{.javascript} if you have an array of strings.
This is nothing new, but a definite improvement over Objective-C.

## Value types & reference types
One thing that might not be immediately obvious is how Swift provides support for both value types and reference types.
In Swift, classes are reference types.
Value types can be defined using the `struct`{.c} and `enum`{.c} constructs.
Both reference types and value types can be used as type parameters.

Many object types in Objective-C have value type counterparts in Swift, including strings, arrays and dictionaries.

## Structures & enumerations
While Swift uses C nomenclature like `enum`{.c} and `struct`{.c}, its enumerations are actually proper [sum types](https://en.wikipedia.org/wiki/Tagged_union).
Your sum types may be sums of [products](https://en.wikipedia.org/wiki/Product_type) and can take type parameters just like classes or functions.

Both enumerations and structures may contain methods.

## Null pointers
[Their inventor](https://en.wikipedia.org/wiki/Tony_Hoare) calls them his [billion dollar mistake](http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare).
They are consistently the cause of most top crashes in the major software products I've worked on.
One alternative to having `null`{.javascript} inhabit every reference type is to use a sum type.

You might have seen the [`Maybe`{.haskell} type](https://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Maybe.html) in [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) or similar types in other languages.
Swift uses the same idea, with some syntactic conveniences:

```javascript
// Either a value of type A or Nothing (e.g. null pointer)
enum Maybe<A> {
  case Nothing
  case Just(A)
}

var a : Maybe<String>

// In Swift, you can write it like this:
var x : String?

// And you can "force unwrap" it with the `!` operator:
x!.someMethod()
```

The compiler can now tell you at compile time if the `Nothing`{.haskell}/`nil`{.javascript} case of an optional type is not handled.
Of course, the `!`{.haskell} operator (equivalent to the [`fromJust`{.haskell} function](https://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Maybe.html#v:fromJust) in Haskell) still lets you explicitly shoot yourself in the foot.
The Swift designers made the choice of translating all Objective-C pointer types to optional types, which makes code uglier in the short term.
Once Swift frameworks make an appearance, safer APIs can be designed that take full advantage of the improved type system.

## Pattern matching
The `switch`{.c} construct---while syntactically similar to its C counterpart---is actually more similar to [pattern matching](https://en.wikipedia.org/wiki/Pattern_matching) in the [ML family](https://en.wikipedia.org/wiki/ML_(programming_language)) of programming languages.
This means the `case`{.c}s can match any type of value, rather than being restricted to constant integral values.
It also lets you extract values as part of the matching process:

```javascript
enum Response {
  case Result(String)
  case Error(Int, String)
}
 
let response1 = Response.Result("Yay")
let response2 = Response.Error(500, "Oh no")
 
switch response1 {
case let .Result(result):
  print("Got a successful response: \(result)")
case let .Error(code, message):
  print("Got an error, code: \(code) message: \(message)")
}
```

## Immutability
By default, value types (defined by the `struct`{.c} and `enum`{.c} constructs) are immutable, while reference types (defined by the `class`{.javascript} construct) are mutable.
There are two ways to declare a variable: the `var`{.javascript} construct---which declares a mutable variable---and the `let`{.javascript} construct---which declares an immutable value:

```javascript
let x = 42
x = 9 // Compile-time error
var y = 42
y = 1 // Works, y is mutable
```

## Functions & closures
While Objective-C supports [closures](https://en.wikipedia.org/wiki/Closure_(computer_programming)) in the form of [blocks](https://en.wikipedia.org/wiki/Blocks_(C_language_extension)), their syntax and semantics differ from those of functions.
In Swift, functions and closures are unified (and you won't need [this](http://fuckingblocksyntax.com)).
There is some shorthand syntax for anonymous functions:

```javascript
// Arguments and body are separated by `in`; no explicit return needed
[1, 2, 3, 4, 5].map({ number in 3 * number })

// You can omit the arguments list and refer to them by number
sort([5, 3, 12, 1, 7], { $0 > $1 })

// If the function is the last argument, you can put outside the arguments list
sort([5, 3, 12, 1, 7]) { $0 > $1 }
```

Personally, I'm not a fan of the numeric arguments or being able to put a function outside of the parentheses if it's the last argument.
The latter especially seems like a weird special case just to be able to shuffle some characters around. 

## Interoperability
Any Objective-C module can be imported and used directly in Swift:

```javascript
import UIKit
// Now we can use the Swiftified framework
let vc = UIViewController()
```

For other C or Objective-C code, you can use what is referred to as a *bridging header*.
This is just a header file that includes all header files you wish to make available in your Swift program:

```c
#import <ABCSomeHeader.h>
#import "ABCSomeOtherHeader.h"
// That's it
```
Create the header file and put the path in `SWIFT_OBJC_BRIDGING_HEADER`.
If you want to use Swift code in an Objective-C project, you have to generate header files from your Swift modules.
Xcode will help you with both of these things if you add C/Objective-C files to a Swift project,
or Swift files to an Objective-C project.

## Bugs & issues
The Swift compiler will periodically remind you that it's not very mature yet.
For example, if you try to implement the equivalent of `data Either a b = Left a | Right b`{.haskell} like so:

```javascript
enum Either<A, B> {
    case Left(A)
    case Right(B)
}
```

You will encounter `Unimplemented IR generation feature non-fixed multi-payload enum layout`.

When trying to interact with a C library,
I found out that there's currently no way to pass a Swift function to a C function that expects a function pointer as one of its arguments.

Another issue I came across is that recursive enumeration types cause a segmentation fault.

## Wrapping up
While Swift doesn't really offer any remarkable innovations in programming language design,
it incorporates many of the advancements made in the past decades into a reasonably simple and approachable language and brings some of them into the mainstream for the first time.

It'll be interesting to see if iOS and Mac software improves as developers move to Swift.
Performance will definitely improve; stability-wise I suspect it depends a lot on the libraries and frameworks: as long as we're still using Objective-C frameworks, not much will be gained.
When real Swift frameworks appear, that take advantage of the improved type system, the situation should improve a lot.

Personally, I think Swift is a huge upgrade compared to Objective-C and I hope the latter can be retired soon.
