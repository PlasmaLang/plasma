# Plasma Language
## *a new programming language*

Plasma is a new programming language for safe and efficient general purpose
use.
It is a statically typed, side-effect free single assignment language
and will have functional programming and concurrent programming features.
It will support deterministic parallel execution.
For a general overview, please visit
[https://plasmalang.org/](https://plasmalang.org/)
It is in early development.

It is free software, Copyright (C) 2015-2022 The Plasma Team, distributed
mostly under the MIT license, see [LICENSE](LICENSE) for details.

![CI](https://github.com/PlasmaLang/plasma/workflows/CI/badge.svg)

## Getting started

This README.md contains some quick info for getting started.
For more complete info please see our
[getting started guide](https://plasmalang.org/docs/getting_started.html).

### Dependencies

Plasma has been tested on Linux, Windows subsystem for Linux 1 and 2 on
x86\_64.

You will need:

* A C compiler (C99 on a POSIX.1-2008 environment), I've been testing with
  GCC.  Clang should also work.
* GNU Make
* [Mercury](https://www.mercurylang.org/).
  A recent stable version is required (22.01.x).
  Plasma's CI currently tests with 22.01.
* asciidoc and source-highlight
* The [ninja build system](https://ninja-build.org), at least version 1.10.

### Mercury installation

The easiest way to install Mercury is to install the
[.deb packages](https://dl.mercurylang.org/deb/) (on Debian, Ubuntu, etc).

Otherwise download Mercury's [source package](https://dl.mercurylang.org)
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
Then ```make install`` to install the tools into ```$PREFIX/bin```.

This compiles and installs the following programs.  Make sure they're in
your ```PATH```.

* src/plzc - The plasma compiler, compiles plasma (```.p```) files to
  plasma modules (```.pzo```)
* src/plzlnk - The plasma linker, links one more more modules (```.pzo```)
  into a plasma program (```.pz```)
* src/plzbuild - The plasma build system
* runtime/plzrun - The runtime system, executes plasma programs (```.pz```).
* src/plzasm - The plasma bytecode assembler.  This compiles textual bytecode
  (```.pzt```) to bytecode (```.pzo```).  It is useful for testing the
  runtime.

There are example plasma programs in ```examples/```.  Running ```plzbuild```
in ```examples/``` will build them.
Each program's bytecode is copied to a file in ```examples/``` eg
```hello.pz```, run them with ```plzrun <bytecode>```.

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
[Discord server](https://discord.gg/x4g83w7tKh) are the best
resources for help.

For bugs or missing information please
[file a bug](https://github.com/PlasmaLang/plasma/issues/new).

