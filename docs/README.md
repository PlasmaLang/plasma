Documentation
=============

We're assuming you are running Linux. For Mac it will probably be similar. Since Plasma is currently not supported on Windows, building the docs on that platform is also not documented.

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

You also need to install [source-highlight](https://www.gnu.org/software/src-highlite/source-highlight.html) to get the C code properly highlighted. Your package manager should also have this.

With these installed, you should be set.

To build the documentation type ``make docs`` in the project's top-level
directory.  This will generate the HTML output.

