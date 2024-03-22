<p align="center">
  <a href="http://dev.stephendiehl.com/fun/">
    <img src="https://github.com/sdiehl/kaleidoscope/raw/master/img/dragon.png"/>
  </a>
</p>

<p align="center">
  <em>A short guide to building a tiny programming language in Haskell with LLVM.</em>
</p>

<p align="center">
  <a href="https://twitter.com/smdiehl">Stephen Diehl</a>
</p>

<p align="center">
  updated by <a href="https://www.linkedin.com/in/tom%C3%A1%C5%A1-z%C3%ADtka-94b933165/">Tomáš Zítka</a>
</p>

Haskell LLVM Tutorial
=====================

[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/zitkat/kaleidoscope/blob/master/LICENSE-MIT)
[![LLVM License](http://img.shields.io/badge/license-llvm-silver.svg)](https://github.com/zitkat/kaleidoscope/blob/master/LICENSE-LLVM)

Read original materials by Stephen online:

* [**HTML**](http://www.stephendiehl.com/llvm)
* [**PDF**](http://www.stephendiehl.com/llvm/tutorial.pdf)
* [**Source Code**](https://github.com/llvm-hs/llvm-hs-kaleidoscope)
* [**Condensed Code**](https://github.com/llvm-hs/llvm-hs-kaleidoscope)


Setup
-----

You will need GHC 8.10 or newer as well as LLVM 9.0. For information on installing LLVM 9.0
on your platform of choice, take a look at the
[instructions posted by the llvm-hs maintainers](https://github.com/llvm-hs/llvm-hs/blob/llvm-4/README.md#installing-llvm).

With Haskell and LLVM in place, you can use Stack to install the necessary Haskell
bindings and compile the source code from each chapter.

### Building with Stack (Recommended)

```bash
$ stack build
```

You can then run the source code from each chapter (starting with chapter 2) as follows:

```bash
$ stack exec chapter2
```
or even load the chapter into GHCi repl

```bash
$ stack ghci kaleidoscope:exe:chapter2
```

### Building with make

The source code for the example compiler of each chapter is included in the ``/src`` folder. With the dependencies
installed globally, these can be built using the Makefile at the root level:

```bash
$ make chapter2
$ make chapter6
```

then

```bash
$ ./chapter6.exe
```
to run the REPL.

A smaller version of the code without the parser frontend can be found in the
[llvm-tutorial-standalone](https://github.com/sdiehl/llvm-tutorial-standalone)
repository. The LLVM code generation technique is identical to chapter 6.

Editing
-------

This is an open source project, patches and corrections always welcome.

The tutorial text in tutorial.md can be rendered using pandoc, use

```bash
$ cabal install pandoc
```

To generate the HTML page:

```bash
$ make tutorial.html
```

A standalone PDF can also be generated with:

```bash
$ make tutorial.pdf
```

License
-------

Text is adapted from the LLVM tutorial and is subsequently licensed under the
LLVM license.

The Haskell source files are released under the MIT license. Copyright (c)
2013-2020, Stephen Diehl, 2023 Tomáš Zítka
