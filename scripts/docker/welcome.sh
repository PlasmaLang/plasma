#!/bin/sh

set -e

cat << END

Welcome to the Plasma-ready docker image
----------------------------------------

This is a docker image ready for Plasma development or testing.  It is based
on debian and you may install additional tools with "apt install".

There is a plasma/ subdirectory here.  Consult the README.md and/or copy
template.mk to build.mk to set build parameters.  When you're ready type
"make" and "make test".  Type "git pull" to update to the latest version.

Vim is configured with Plasma and Mercury language plugins.

See:

    https://github.com/PlasmaLang/plasma/tree/master/scripts/docker

for more info or to contribute inprovements.

END

exec /bin/bash

