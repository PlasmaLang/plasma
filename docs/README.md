Documentation
=============

We're assuming you are running Linux. For Mac it will probably be similar. Since Plasma is currently not supported in Windows, building the docs on that platform is also not documented.

You will need a working copy of AsciiDoc on your PC to build the documentation.
For Ubuntu, it is simply a matter of typing

```shell
sudo apt-get install asciidoc
```

and you should be set. Similarly in other distributions.

We will provide a Makefile in the docs/ directory to easily build the right kind of documentation. This is still a work in progress for now.