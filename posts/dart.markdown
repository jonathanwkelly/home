---
date:         2011-10-13
title:        Darting around
description:  Taking Google’s new programming language out for a spin on Mac OS X.
author:       Jonas Westerlund
tags:         clang, dart, javascript, llvm, os-x, xcode
---

Collecting some stuff I come across while trying out [Dart](http://www.dartlang.org/).
Post will be updated with findings, thoughts and stuff.

## Static runtime binaries

Building Dart currently does not work out of the box on Mac OS X 10.7. But never fear, binaries are here.

- [Debug ia32](/files/dart/Debug_ia32/dart)
- [Release ia32](/files/dart/Release_ia32/dart)

I will add x64 binaries as soon as they work.

## Notes on building

If you want to build your own Dart on Mac OS X 10.7, add another option to the list in `build.py` on line 108, it will look something like this.

```python
args = ['xcodebuild',
        '-sdk',
        'macosx10.6',
        (...)
        ]
```
 
You will also need Java. If you would rather not install that, see above.

Since Xcode 4.2, you also need to change the compiler used, in `configurations_xcode.gypi`.
Change the line that reads `'GCC_VERSION': '4.2'` to `'GCC_VERSION': 'com.apple.compilers.llvmgcc42`.

Remember to run `gclient runhooks` after this, to re-generate the files needed.

Now you should be able to run `build.py --arch=ia32 --mode=release` to build the release binaries.

## Pertinent links

- [Official website](http://dartlang.org/)
- [Project at Google Code](https://code.google.com/p/dart/)
- [Google Groups](https://groups.google.com/a/dartlang.org/)
- [Language specification](http://www.dartlang.org/docs/spec/latest/dart-language-specification.pdf)

## Updated <time datetime="2012-10-13">october 13, 2012</time>

After taking Dart for a spin, reading the documentation and specification, and trying out the major features of the language, I had no desire to make further use this language.
Checking in on it every now and then for the past year has not changed this initial conclusion.

While it is certainly more well-designed than JavaScript, that's not exactly a major accomplishment.
I say that not to ridicule JavaScript; anyone expecting greatness [in ten days](http://www.jwz.org/blog/2010/10/every-day-i-learn-something-new-and-stupid/#comment-1021) is deluding themselves.
I don't feel that it's even a big enough improvement to warrant the cost of switching, in the unlikely event of major adoption by the big browsers.

For use outside of browsers, it feels like a step back compared to many contemporary languages.
Its type system is [broken by design](/files/dart-broken.mov), and it doesn't really offer anything unique or compelling to make up for it.
