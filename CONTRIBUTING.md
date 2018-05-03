
# Plasma Contributors' Information

This file contains information for potential and current Plasma
contributors.

## Summary and legal stuff

* We prefer github pull requests or patches mailed to the
  [developers' mailing list](https://www.plasmalang.org/lists/listinfo/dev).
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

## What to contribute

The project is at an early stage and therefore we are prioritising work that
makes the language useful above things like adding compiler optimisations.
Additionally to make bootstrapping easier (into a self-hosted language) we
want to avoid adding code that is not necessary and will later need to be
re-written.  The project [roadmap](http://www.plasmalang.org/roadmap.html)
is a good place to look to know what our current and near-future focus is.

Before starting it is a good idea to
[discuss your ideas with us](https://www.plasmalang.org/lists/listinfo/dev),
we may be able to give you some pointers or let you know what kinds of
problems you may encounter.  Complex changes, especially those that require
some design decisions should be discussed before beginning work.  For
example we might not be interested n making the language weakly typed and
discussing this beforehand may avoid disappointment later.  Ultimately we
want you to enjoy working with Plasma and that means making the most of your
development time.

### Suggestions and good first bugs

If you're looking for a suggestion of what to contribute
look at the
[current open github issues](https://github.com/PlasmaLang/plasma/issues) or
[project board](https://github.com/orgs/PlasmaLang/projects/1)
,
If you find a bug with the good-first-bug label, this indicates that it is
a good bug for getting comfortable with the project, and
*not* necessarily for learning programming.

Also there are many `TODO` and `XXX` notes in the source code, which things
that are not handled.  Search for the strings `TODO` and `XXX`.  Keep in
mind that there may be good reasons why these are not yet handled, eg: it
may depend on other incomplete work.

If you find something undocumented whose behaviour is unlikely to change,
consider filling in that part of the documentation.

When reading code if something isn't clear, please ask us about it,
this is a good hint that we should have written (better) code comments.

## How to contribute

Contributions are valuable and we want to make contributing as easy as
possible.  We can do this by providing support to contributors, especially
new contributors.  This can include providing feedback on your patches.
However by following the guidelines below contributors can decrease the
amount of support required, which gives everyone more time to write code and
contribute.

### Before beginning

It is best to start each piece of work on a new git branch.  Create a branch
off of master and commit your changes there as you go.

### Making your changes

If making a series of patches, try to organise the patches so that each
patch makes sense on its own.  Git has many features for doing this
including cherry picking and rebasing.

Code contributions should follow the style guides as much as possible.
Deviations that make code more readable are permitted.
The guides are
[Mercury style guide](http://www.plasmalang.org/docs/Mercury_style.html) and
[C style guide](http://www.plasmalang.org/docs/C_style.html).

TODO: Provide information about project structure.

Spell check and test your work, use ```make test``` for the latter.  Each patch
should, when applied in series, pass the test suite.

### Documenting your changes

User-visible changes including new options, features and behaviours must be
documented.  For now options are documented in the --help text of each
program.  While designs and concepts are documented in one of the files in
the the docs directory, these files are asciidoc text files.

### Submitting your changes

All code contributions must be made under the the approprite license:

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

  path/file1:
      Specific changes in file1.

  path/file2:
      Specific changes in file2.

We provide a .gitmessage in the root of the repository.
Run this command to start using the new commit message template:

```
git config --local commit.template /path/to/repo/.gitmessage
```

```
```components``` is one or more parts of the system.  This helps people
identify (in mailing lists, change logs etc) what kind of change has been
made at a glace.  It also helps people and software search for changes.
Current components are:

* pz: the PZ file format,
* pzrun: the runtime,
* pzasm: the PZ assembler,
* plasmac: the compiler generally,
* plasmac/parse: the first phase: parsing.
* plasmac/ast: the second phase: the AST and operations on it,
* plasmac/pre: the third phase: the pre-core representation and operations,
* plasmac/core: the fourth phase: the core representation and operations,
* plasmac/pz: the fitht phase: the PZ code generator,
* docs: documentation,
* build: the build system,

Sometimes it makes sense to pick the component with the most sagnificant
changes rather than listing all of them.  This is typical for changes to the
compiler.

Each file should be listed with more detailed information.  Take a look at
previous changes for examples.

We accept contributions via pull request on github, or via e-mailed patches.
If you choose to use e-mailed patches then the ```git format-patch``` and/or
```git send-email``` tools can be used to generate nice e-mails, however
this is not required, ```diff -uNr``` is sufficient.
E-mailed patches should be sent to the
[dev](https://www.plasmalang.org/lists/listinfo/dev) mailing list.

TODO: Provide suitable step-by-step instructions.

We aim to act on your changes reasonbly quickly.  However this is something
people do in their spare time, they may be busy with other aspects of their
lives and not reply for several days.  We will provide feedback and guidance
where appliable.  As stated abouve, we want you to enjoy working with Plasma
and that means we will try to help you make the most of your development
time.
