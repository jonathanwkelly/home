---
title:  Invalid form validation
author: Jonas Westerlund
tags:   javascript, regular-expressions, unicode
---

Every now and then I end up on a website that, for no good reason, rejects something I enter into a form.
Usually, bad [regular expression](https://developer.mozilla.org/en/Core_JavaScript_1.5_Guide/Regular_Expressions) validation is causing this.
It is somewhat difficult to write [Unicode](http://en.wikipedia.org/wiki/Unicode)-aware regular expressions in JavaScript, so these expressions often consider a letter to be A to Z and nothing else.
So if you live in Umeå, or your name is José, you may be out of luck.
Why, then, is it hard to write good regular expressions? Often, developers just don't think about these issues, and if they do, they may be hindered by JavaScript's lack of Unicode features.

## Unicode in JavaScript

The [ECMAScript](http://en.wikipedia.org/wiki/ECMAScript) specification defines a string as a sequence of [UTF-16](http://en.wikipedia.org/wiki/UTF-16) code units. This implementation detail is fully exposed to programmers, which means they must sometimes deal with these code units directly.

If you are unfamiliar with UTF-16, it is one of several encodings for Unicode.
It is a variable-length encoding, so a single code point can be represented by one or two 16-bit code units.
Since strings are defined in terms of code units, this means that the value of their `length` property will not necessarily reflect the number of characters in the string, since a single character may be represented by two code units.
It also means that use of methods like `substring` can break characters, since they also operate on the code unit level.

In practice, this is rarely a problem, since the characters for most modern languages are assigned to the [basic multilingual plane](http://en.wikipedia.org/wiki/Basic_Multilingual_Plane#Basic_Multilingual_Plane), and can be represented by a single code unit.

## Unicode in regular expressions

A more problematic area is JavaScript regular expressions.

Unicode characters are organized into categories. There are categories for letters, numbers, punctuation and so on.
Some regular expression engines support these categories, so that you can easily match anything that belongs to a certain category.

JavaScript regular expressions currently do not support them.
They do support some [character classes](http://www.regular-expressions.info/charclass.html), but these are not Unicode-aware. This means that the [word character](http://www.regular-expressions.info/charclass.html#shorthand) class, for example, will only match the characters A to Z (and their lowercase counterparts), 0 to 9, and the underscore character.

## Form validation with regular expressions

Regular expressions are often used to perform client-side form validation.
HTML5 even introduced a new attribute for input elements, [`pattern`](http://www.whatwg.org/specs/web-apps/current-work/multipage/common-input-element-attributes.html#the-pattern-attribute), which contains a JavaScript regular expression that will be used to validate the input's value.

Since it is a bit difficult to write Unicode-friendly regular expressions in JavaScript, most developers don't.

A way to sidestep this issue is to simply be very permissive when validating form input.
A quick Google search for "javascript name validation regular expression" will find code like `var fnameRegxp = /^([a-zA-Z]+)$/`{.javascript}.
This will obviously not work very well, as it excludes a lot of letters and accents.
Instead, you could simply consider it valid if the user has entered anything at all, and not try to check if it contains letters only.

Another solution, if you really need it, is to use a library that adds these missing features. [XRegExp](http://xregexp.com/) is such a library, it provides a more usable implementation of regular expressions, with support for Unicode categories (through a [plugin](http://xregexp.com/plugins/)), named capture, and more.
With this library, you could instead write `XRegExp("^\\p{L}+$")`{.javascript} and it would at least match any letter, not just A to Z.

But still, don't be too restrictive with your validation scripts, you will more than likely reject valid input.
As an example, [this regular expression](http://www.ex-parrot.com/pdw/Mail-RFC822-Address.html) is what would be required to validate that an email address conforms to [RFC 822](http://www.ietf.org/rfc/rfc0822.txt). And if you google for one, you will [find](http://javascript.about.com/library/blre.htm) [plenty](http://www.marketingtechblog.com/programming/javascript-regex-emailaddress/) of [expressions](http://www.devx.com/tips/Tip/35130) that reject valid addresses.
You can see how difficult it is to write a correct one, and how easy it is to write an incorrect one, when you try to be restrictive.

If you must validate something at all, and absolutely want to use a regular expression to do it, make it as permissive as possible.
