
# Plasma Contributors' Information

Contributions are valuable and we want to make contributing as easy as
possible.  We can do this by providing support to contributors, especially
new contributors.  This can include providing feedback on your patches among
other things.  However by following the guidelines below contributors can
decrease the amount of support required, which gives everyone more time to
write code and contribute.


## Summary (TL;DR)

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


## Before beginning

It is best to start each piece of work on a new git branch.  Create a branch
off of master and commit your changes there as you go.

For complex changes, especially those that require some design decisions we
recommend discussing them with the other developers before beginning work.
For small changes this isn't necessary.  However large changes, especially
those that require design decisions, are important to discuss to avoid
wasted work.  We want you to enjoy working with Plasma and that means making
the most of your development time.  Discussion also helps avoid several
developers stepping on each-others toes when working on the same or related
code.  The best way to get in contact for these discussions is on the
[dev](https://www.plasmalang.org/lists/listinfo/dev).
For realtime conversation you may find us online in the IRC channel
(#plasmalang on [Freenode](http://freenode.org)).
If we're not online please be patient, we may not be in your timezone or
busy with other aspects of our lives.


## Making your changes

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


## Submitting your changes

All contributions must be made under the MIT license.  No transfer of
copyright or other rights or permissions is required.  All contributors
should be listed in the AUTHORS file, and all contributors with copyrights
_must_ be listed.

Log messages should follow the style:

  [component(s)] Title

  Description

  path/file1:
      Specific changes in file1.

  path/file2:
      Specific changes in file2.

```components``` is one or more parts of the system.  This helps people
identify (in mailing lists, change logs etc) what kind of change has been
made at a glace.  It also helps people and software search for changes.
Current components are:

* pz: the PZ file format,
* pzrun: the runtime,
* pzasm: the PZ assembler,
* plasmac: the compiler,
* docs: documentation,
* refactor: refactoring change only.

Each file should be listed with more detailed information.  Take a look at
previous changes for examples.

We accept contributions via pull request on either github or bitbucket,
or via e-mailed patches.  If you choose to use e-mailed patches then the
```git format-patch``` and/or ```git send-email``` tools can be used to
generate nice e-mails, however this is not required, ```diff -uNr``` is
sufficient.
E-mailed patches should be sent to the
[dev](https://www.plasmalang.org/lists/listinfo/dev) mailing list.

TODO: Provide suitable step-by-step instructions.

We aim to act on your changes reasonbly quickly.  However this is something
people do in their spare time, they may be busy with other aspects of their
lives and not reply for several days.  We will provide feedback and guidance
where appliable.

## Other ways to help.

We keep a list of TODO items in docs/todo.txt and online at
[TODO items](http://www.plasmalang.org/docs/todo.html).
This is intended to be a list of small-ish tasks that should not be
forgotten.  Larger scale items like "Write a compiler" are shown in the
[Roadmap](http://www.plasmalang.org/roadmap.html).

Search the source code for the strings "XXX" or "TODO" for things that must
be handled.  Keep in mind that there may be good reasons why these are not
yet handled, eg: other work needs doing first.  Therefore it is a good idea
to [discuss this with us](https://www.plasmalang.org/lists/listinfo/dev).

If you find something undocumented whose behaviour is unlikely to change,
consider filling in that part of the documentation.

When reading code if something isn't clear, please
[ask](https://www.plasmalang.org/lists/listinfo/dev).  us about it, this is
also a good hint that we should have written (better) code comments.


