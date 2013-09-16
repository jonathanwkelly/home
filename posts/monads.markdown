---
date:         2013-09-17
title:        Yet another monad introduction
description:  An introduction to monads, based on the definition in the Haskell standard library.
author:       Jonas Westerlund
tags:         functional, haskell, monad, programming
---

Monads have an undeserved reputation of being difficult to understand.
To make matters worse, there is a widespread misconception that you need to master the subject before you can proceed in your functional programming adventures (Haskell in particular).

This post is not intended to give you a deep understanding of monads,
but to demonstrate that there is nothing mysterious going on,
and that you can happily code away without getting hung up on it.
I will try to avoid Haskell jargon, and provide analogous code in other languages where possible.

## Summary
By going through the definition of a monad, we will see that:

- A monad is an interface which declares two primary methods
- A monad implementation should obey three simple laws

That's it, really. If you stop reading here, that's fine.
If you want to look a bit closer, read on.

## A quick tour of Haskell syntax
If you are not familiar with Haskell syntax, here is a brief overview of the constructs used in this post.

A type signature describes the types of the inputs and output of a function, and is written `foo :: String -> Int`{.haskell}.
It's similar to function prototypes in C, like `int foo(char *)`{.c}.
Lowercase type identifiers are *type variables*, and are similar to generic types in Java, or type parameters in C++ templates (but don't let that intimidate you).

Juxtaposition denotes function application, `f a b`{.haskell}.
That would be `f(a, b)`{.python} in Python or JavaScript.

Lambdas are written `\a b -> expr`{.haskell}, the `\`{.haskell} is meant to be an ASCII-friendly version of `λ`.

A type class describes an interface, much like interfaces in Java and similar languages.
They take the form:

```haskell
class Interfoo i where                -- `i` is any type implementing the interface
  aMethod       :: i a -> i b -> i b  -- `a` and `b` are type parameters
  anotherMethod :: a -> i a
```

They simply list the operations and their type signatures.
They can also contain default implementations, like interfaces in many other languages, and those look like any other function definition.

## The monad interface definition
A monad can be thought of as an interface with two methods: *bind*, written `(>>=)`{.haskell}, and `return`{.haskell}.

```haskell
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b  -- Pronounced "bind"
  return :: a   -> m a                -- Not a keyword, just an unfortunate name
```

The definiton in Haskell's `Control.Monad`{.haskell} contains two additional methods, `(>>)`{.haskell} and `fail`{.haskell}.
The former is defined in terms of `(>>=)`{.haskell}, and the latter is an historical accident, and will not be covered.

The signature of `(>>=)`{.haskell} tells us that it takes a parameter `m a`{.haskell}, any monadic value of type `a`{.haskell},
and a parameter `(a -> m b)`{.haskell}, a function from `a`{.haskell} to `m b`{.haskell}.
The actual types do not matter, but where the same names are used, the types must be the same.

## The three monad laws
A monad implementation should obey three laws: *left identity*, *right identity*, and *associativity*.
They can be defined like this:

```haskell
return a >>= f                  = f a               -- Left identity
m        >>= return             = m                 -- Right identity
m        >>= (\x -> f x >>= g)  = (m >>= f) >>= g   -- Associativity
```

The associative law might look a bit strange, because of the assymetry.
Here is another formulation:

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
```

The law can now be expressed as `(f >=> g) >=> h = f >=> (g >=> h)`{.haskell},
just like how `(a + b) + c = a + (b + c)`{.haskell}, like you learned in elementary school.

Of course, you don't need to worry much about any of this until you implement your own monads.
Let's look at some monad implementations.

## The list monad implementation
Lists should be familiar to anyone who has programmed a little bit in any language.
One possible way to implement the monad interface for the list type is this:

```haskell
instance Monad [] where
  m >>= f   = concatMap f m
  return a  = [a]
```

If the list syntax obscures the correspondence to the monad interface,
just think of `[a]`{.haskell} as `List a`{.haskell}.
The signature `concatMap :: (a -> [b]) -> [a] -> [b]`{.haskell} is exactly like the signature of bind (except for the argument order),
with the list type taking the role of the monad `m`{.haskell}.
Going from a simple `a`{.haskell} to a monadic `m a`{.haskell} is as simple as `\a -> [a]`{.haskell}.

## The maybe monad implementation
The data type `Maybe a`{.haskell} is used to represent optional values, and is defined like this:

```haskell
data Maybe a = Nothing | Just a   -- `Nothing` and `Just` are constructors
```

To implement the monad interface for the `Maybe`{.haskell} type, we can write:

```haskell
instance Monad Maybe where
  (Just a) >>= f  = f a
  Nothing  >>= _  = Nothing
  return a        = Just a
```

This makes use of pattern matching to provide two equations for `(>>=)`{.haskell}.
You can think of it as syntactic sugar for the following case expression:

```haskell
m >>= f = case m of
  (Just a)  -> f a
  Nothing   -> Nothing
```

So `m >>= f`{.haskell} gives us `Nothing`{.haskell} if the value is `Nothing`{.haskell},
and applies `f`{.haskell} to the value inside `Just a` otherwise, returning a new value of type `Maybe a`{.haskell}.
Great!

## Concluding thoughts
If this is all new to you, it might seem like a lot of weird stuff.
But if you look at the actual definitions, it's very little (and very simple) code.

I have only covered two monad implementations here --- two of the most mundane ones --- but the reason the monad interface exists is that it's an incredibly common pattern.
Lots of things that may look completely different at first glance end up having monad implementations.

What this lets us do is write code that work for *all monads*, letting us re-use code in different scenarios.
As monads are so pervasive, this happens quite often.

It also works the other way around: if you can implement the monad interface for your own data types,
suddenly you have a wealth of code that will work with it.
