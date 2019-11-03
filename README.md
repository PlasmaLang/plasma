# Plasma Language
## *a new programming language*

Plasma is a new programming language for safe and efficent general purpose
use.
It is a statically typed, side-effect free single assignment language
and will have functional programming and concurrent programming features.
It will support deterministic parallel execution.
For a general overview, please visit
[https://plasmalang.org/](https://plasmalang.org/)
It is in early development.

It is free software, Copyright (C) 2015-2019 The Plasma Team, distributed
mostly under the MIT license, see [LICENSE](LICENSE) for details.

## Github and ICE

It came to light recently that Github have a contract with ICE, the
US government department responsible for separating families and torturing
children who have the misfortune to try to enter the USA.

I (Paul Bone) am investigating options other than github and will move
hosting of Plasma away from github (pending a suitable alternative and
migration plan) if github do not end their contract with ICE.

I do not pay github for any services.

## Getting started

This README.md contains some quick info for getting started.
For more complete info please see our
[getting started guide](https://plasmalang.org/docs/getting_started.html).

### Dependencies

You will need:

* A C compiler (C99 on a POSIX.1-2008 environment), I've been testing with
  GCC.  Clang should also work.
* [Mercury](https://www.mercurylang.org/).  14.01.1 at a minimum,
  but a newer 'rotd' version (2019-08-30 is known to work) is recommended
  for compatibility with newer C compilers and critical if you use
  musl-based system.
  There are rumours that newer versions aren't compatible on OS X,
  but I don't have any solid info.
* Asciidoc

### Mercury installation

The easiest way to install Mercury is to install the
[.deb packages](https://dl.mercurylang.org/deb/) (on Debian, Ubuntu, etc).

Otherwise download Mercury's [source pakcage](https://dl.mercurylang.org)
and follow the
installation instructions in the
[INSTALL](https://github.com/Mercury-Language/mercury/blob/master/.INSTALL.in)
file.
You'll need at least version 14.01.1, but newer 'rotd' versions have better
C compiler and C library support.
ROTD 2018-11-14 is known to work on 64bit Linux for Plasma.
We've made some
[notes about grades](https://plasmalang.org/docs/grades.html)
that may help with choosing which grades you may need.
There is also a
[README.bootstrap](https://github.com/Mercury-Language/mercury/blob/master/README.bootstrap)
file with Mercury bootstrapping information if you wish to do that, it may
also provide some additional explaination.

### Usage

Make any changes you may need in the Makefile, everything configurable is
towards the top of this file.

Use ```make``` in the root directory to build the project.

You should get:

* src/plzc - The plasma compiler, compiles plasma (```.p```) files to
  plasma bytecode (```.pz```)
* runtime/plzrun - The runtime system, executes plasma bytecode (```.pz```)
  files.
* src/plzasm - The plasma bytecode assembler.  This compiles textual bytecode
  (```.pzt```) to bytecode (```.pz```).  It is useful for testing the
  runtime.

There are example plasma programs in ```examples/```.  Running ```make
test``` will execute these programs as part of the test suite to ensure that
things are working correctly.

### Layout

* [docs](docs) - Documentation
* [examples](examples) - Example Plasma programs
* [runtime](runtime) - Runtime system (C code)
* [scripts](scripts) - Some scripts to aid developers
* [src](src) - The compiler and other tools
* [tests](tests) - The test suite (in addition to some of the files in
  [examples](examples))

## Contributing

Please see [CONTRIBUTING.md](CONTRIBUTING.md) and
[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).
For detailed information including what to work on please see
[Contributing to Plasma](https://plasmalang.org/docs/contributing.html) in
our documentation.

## Getting help

If you're stuck and  the [Documentation](https://plasmalang.org/docs/)
doesn't contain the answer or clue you need or you're struggling to find it.
Please ask for help.
The [Contact](https://plasmalang.org/contact.html) page of the website lists
all the ways you can get in touch.
In particular the
[Plasma Help mailing list](https://plasmalang.org/lists/listinfo/help)
and
[IRC channel](https://plasmalang.org/contact.html#irc) are the best
resources for help.

For bugs or missing information please
[file a bug](https://github.com/PlasmaLang/plasma/issues/new).

