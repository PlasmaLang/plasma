#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4 ft=make
#

# Where programs are installed
PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
DOCDIR=$(PREFIX)/share/doc/plasma
VERSION=dev

# The number of parallel jobs the Mercury compiler should spawn.
JOBS=$(shell if X=$$(nproc 2>&1); then echo $$X; else echo 1; fi)

# How the Mercury compiler should be called.  You may need to adjust this if
# it is not in your path.
MMC_MAKE=mmc --make -j$(JOBS) --use-grade-subdirs

# How the C compiler should be called.  gcc and clang should both work.
# Note that Mercury has its own configuration for its C backend, which is
# not, and must not be changed here.
# Note also that we'd normally define _DEFAULT_SOURCE once in
# C_CXX_FLAGS_BASE, but Mercury also defines this so we avoid a warning by
# listing it twice for C_ONLY then CXX_ONLY.
CC=gcc
CXX=g++
C_CXX_FLAGS_BASE=-D_POSIX_C_SOURCE=200809L
C_ONLY_FLAGS=-std=c99 -D_DEFAULT_SOURCE
CXX_ONLY_FLAGS=-std=c++11 -fno-rtti -fno-exceptions -D_DEFAULT_SOURCE
MCFLAGS=

# gcc and probably clang support dependency tracking.  If your compiler
# doesn't uncomment the 2nd line.
DEPDIR=.dep
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$(basename $*).Td

# How to install programs, specify here the owner, group and mode of
# installed files.
INSTALL=install
INSTALL_STRIP=install -s
INSTALL_DIR=install -d

# How to call asciidoc (optional). A full path or any flags here won't work
# without other changes to the makefile.
ASCIIDOC=asciidoc

# How to call clang-format (optional)
CLANGFORMAT=clang-format-10

# This base configuration works on Linux but you may need to change them for
# other systems / compilers.
C_CXX_FLAGS=$(C_CXX_FLAGS_BASE) -O1 -Wall
BUILD_TYPE=rel

# This is a suitable build for development.  It has assertions enabled in
# the C code some of which are slow, so they shouldn't be used for
# performance measurement.  Comment it out to use one of the optimised
# builds below.
#
# Note to maintainers: When Plasma is actually "used" we should change this
# default and provide a better way for developers to setup a "dev" build
# with assertions and other checks.

# Development build options
MCFLAGS+=--warn-dead-procs
C_CXX_FLAGS+=-Werror -DDEBUG -DPZ_DEV
BUILD_TYPE=dev

