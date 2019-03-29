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

### Dependencies

You will need:

* A C compiler (C99 on a POSIX.1-2008 environment), I've been testing with
  GCC.  Clang should also work.
* [Mercury](https://www.mercurylang.org/).  14.01.1 at a minimum,
  but a newer 'rotd' version (2018-11-14 is known to work) is recommended
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
* runtime/pzrun - The runtime system, executes plasma bytecode (```.pz```)
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
* [src](src) - The compiler and other tools
* [tests](tests) - The test suite (in addition to some of the files in
  [examples](examples))

### Contributing

Please see [CONTRIBUTING.md](CONTRIBUTING.md) and
[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

