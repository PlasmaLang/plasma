Documentation
=============

We're assuming you are running Linux. For Mac it will probably be similar. Since Plasma is currently not supported in Windows, building the docs on that platform is also not documented.

You will need a working copy of AsciiDoc on your PC to build the documentation.
For Ubuntu, it is simply a matter of typing

```shell
sudo apt-get install asciidoc
```

For Fedora: 

```shell
sudo dnf install asciidoc
```

For other distros, check your package manager.

You also need to install https://www.gnu.org/software/src-highlite/source-highlight.html[source-highlight] to get the C code properly highlighted.

With these installed, you should be set.

We will provide a Makefile in the docs/ directory to easily build the right kind of documentation. This is still a work in progress for now.