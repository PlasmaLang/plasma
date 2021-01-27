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

It is free software, Copyright (C) 2015-2021 The Plasma Team, distributed
mostly under the MIT license, see [LICENSE](LICENSE) for details.

![CI](https://github.com/PlasmaLang/plasma/workflows/CI/badge.svg)

## Github and ICE

It came to light in 2019 that Github have a contract with ICE, the US
government department responsible for separating families and torturing
children who have the misfortune to try to enter the USA.

Github, you can (and should) do better and I (Paul Bone) will never be using
any paid features while you contract with ICE.

## Getting started

This README.md contains some quick info for getting started.
For more complete info please see our
[getting started guide](https://plasmalang.org/docs/getting_started.html).

### Dependencies

You will need:

* A C compiler (C99 on a POSIX.1-2008 environment), I've been testing with
  GCC.  Clang should also work.
* [Mercury](https://www.mercurylang.org/).
  The latest stable version is required, older versions may also work.
  Plasma's CI currently tests with ROTD 2020-05-31.
* Asciidoc

### Mercury installation

The easiest way to install Mercury is to install the
[.deb packages](https://dl.mercurylang.org/deb/) (on Debian, Ubuntu, etc).

Otherwise download Mercury's [source pakcage](https://dl.mercurylang.org)
and follow the
installation instructions in the
[INSTALL](https://github.com/Mercury-Language/mercury/blob/master/.INSTALL.in)
file.
We've made some
[notes about grades](https://plasmalang.org/docs/grades.html)
that may help with choosing which grades you may need.
There is also a
[README.bootstrap](https://github.com/Mercury-Language/mercury/blob/master/README.bootstrap)
file with Mercury bootstrapping information if you wish to do that, it may
also provide some additional explaination.

### Usage

Copy `template.mk` to `build.mk` and edit it to make any build configuration
changes you need.

Use ```make``` in the root directory to build the project.

You should get:

* src/plzc - The plasma compiler, compiles plasma (```.p```) files to
  plasma modules (```.pzo```)
* src/plzlnk - The plasma linker, links one more more modules (```.pzo```)
  into a plasma program (```.pz```)
* runtime/plzrun - The runtime system, executes plasma programs (```.pz```).
* src/plzasm - The plasma bytecode assembler.  This compiles textual bytecode
  (```.pzt```) to bytecode (```.pzo```).  It is useful for testing the
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

