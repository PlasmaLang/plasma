
# Plasma Contributors' Information

This file contains information for potential and current Plasma
contributors.

## Summary and legal stuff

* We prefer github pull requests or patches mailed to the
  [developers' mailing list](https://plasmalang.org/lists/listinfo/dev).
  If you need to discuss a security issue confidently you can e-mail
  plasma at plasmalang dot org
* The license of your contribution must match the project's licenses:
  * Code: MIT
  * Docs: CC BY-SA 4.0
  * Build scripts, tests, and sample code: Unlicense
* No contributor agreement is required, you retain the copyright for your
  contribution.
* Add your name to the AUTHORS file.
* Please follow the style guides as much as possible (see below)
* Please format your log messages following the log message style (see
  below)
* By submitting a PR you acknowledge these terms and agree to the
  [Code of Conduct](CODE_OF_CONDUCT.md)
* By opening an issue/commenting/messaging you agree to the
  [Code of Conduct](CODE_OF_CONDUCT.md)

## What and how to contribute

Full contributing information is provided [in the contributors'
guide](https://plasmalang.org/docs/contributing.html).

## Submitting your changes

All code contributions must be made under the appropriate license:

* Code: MIT
* Docs: CC BY-SA 4.0
* Build scripts, tests, and sample code: Unlicense

No transfer of copyright or other rights or permissions is required.  All
contributors should be listed in the AUTHORS file, and all contributors with
copyrights _must_ be listed.

Log messages should follow the style:

```
  [component(s)] Title

  Description

  Any other changes including changes that were needed to support this
  change or followed as a concequence of this change.
```

We provide a .gitmessage in the root of the repository.
Run this command to start using the new commit message template:

```
git config --local commit.template /path/to/repo/.gitmessage
```

```components``` is one or more parts of the system.  This helps people
identify (in mailing lists, change logs etc) what kind of change has been
made at a glace.  It also helps people and software search for changes.
Current components are:

* pz: the PZ file format,
* rt: the runtime generally,
* rt/interp: the bytecode interpreter,
* rt/gc: the garbage collector,
* asm: the PZ assembler,
* compiler: the compiler generally,
* compiler/parse: the first phase: parsing.
* compiler/ast: the second phase: the AST and operations on it,
* compiler/pre: the third phase: the pre-core representation and operations,
* compiler/core: the fourth phase: the core representation and operations,
* compiler/pz: the fitht phase: the PZ code generator,
* docs: documentation,
* build: the build system,
* tests: the test suite,

Sometimes it makes sense to pick the component with the most sagnificant
changes rather than listing all of them.  This is typical for changes to the
compiler.

Each patch should contain a single change and changes required by that
change (should compile and pass tests).  Changes may e rolled together when
they're trivial related changes (eg, multiple spelling fixes).

