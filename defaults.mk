#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4 ft=make
#

# The number of parallel jobs the Mercury compiler should spawn.
JOBS=8

# How the Mercury compiler should be called.  You may need to adjust this if
# it is not in your path.
MMC_MAKE=mmc --make -j$(JOBS)
MCFLAGS=--use-grade-subdirs

# How the C compiler should be called.  gcc and clang should both work.
# Note that Mercury has its own configuration for its C backend, which is
# not, and must not be changed here.
CC=gcc
CXX=g++

# gcc and probably clang support dependency tracking.  If your compiler
# doesn't uncomment the 2nd line.
DEPDIR=.dep
DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$(basename $*).Td

# How to call asciidoc (optional). A full path or any flags here won't work
# without other changes to the makefile.
ASCIIDOC=asciidoc

# How to call clang-format (optional)
CLANGFORMAT=clang-format

# How to call indent (optional)
INDENT=indent

# This base configuration works on Linux but you may need to change them for
# other systems / compilers.
C_CXX_FLAGS=-O1 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE
C_CXX_WARN_FLAGS=-Wall
C_ONLY_FLAGS=-std=c99
CXX_ONLY_FLAGS=-std=c++11 -fno-rtti -fno-exceptions
BUILD_TYPE=release

# This is a suitable build for development.  It has assertions enabled in
# the C code some of which are slow, so they shouldn't be used for
# performance measurement.  Comment it out to use one of the optimised
# builds below.
#
# Note to maintainers: When Plasma is actually "used" we should change this
# default and provide a better way for developers to setup a "dev" build
# with assertions and other checks.

# Development build options
MCFLAGS2=--warn-dead-procs
C_CXX_WARN_FLAGS+=-Werror -DDEBUG -DPZ_DEV
BUILD_TYPE=dev

