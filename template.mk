#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4 ft=make
#

# Basic configuration
# ===================
# 
# To configure Plasma copy this file `template.mk` to `build.mk` and then
# modify it there.
#
# Sensible defaults are already set by defaults.mk, so to change them
# uncomment and modify the settings in this file to override those defaults.
#

# How the Mercury compiler should be called.  You may need to adjust this if
# it is not in your path.
# MMC_MAKE=mmc --make -j$(JOBS) --use-grade-subdirs

# The number of parallel jobs the Mercury compiler should spawn.
# JOBS=8

# How the C compiler should be called.  gcc and clang should both work.
# Note that Mercury has its own configuration for its C backend, which is
# not, and must not be changed here.
# CC=gcc
# CXX=g++

# gcc and probably clang support dependency tracking.  If your compiler
# doesn't uncomment the 2nd line.
# DEPDIR=.dep
# DEPFLAGS=-MT $@ -MMD -MP -MF $(DEPDIR)/$(basename $*).Td

# How to call asciidoc (optional). A full path or any flags here won't work
# without other changes to the makefile.
# ASCIIDOC=asciidoc

# How to call clang-format (optional)
# CLANGFORMAT=clang-format

# How to call indent (optional)
# INDENT=indent

# Detailed build options
# ----------------------
#
# The following settings are closely related and therefore we provide
# suggestions in groups, depending on what type of build you want.
#
# Note that there are also some build parameters in src/Mercury.options
#
# We start with a base configuration, these work on Linux but you may need
# to change them for other systems / compilers.
#
# This is a suitable build for development.  It has assertions enabled in
# the C code some of which are slow, so they shouldn't be used for
# performance measurement.  Comment it out to use one of the optimised
# builds below.
#
# MCFLAGS=--warn-dead-procs
# C_CXX_FLAGS=-O1 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE
# C_CXX_WARN_FLAGS=-Wall
# C_CXX_WARN_FLAGS+=-Werror -DDEBUG -DPZ_DEV
# C_ONLY_FLAGS=-std=c99
# CXX_ONLY_FLAGS=-std=c++11 -fno-rtti -fno-exceptions
# BUILD_TYPE=dev

# You can uncomment _at most one_ of the following sets of options, or write
# your own.

# Enable C and Mercury debugging.
# MCFLAGS=--grade asm_fast.gc.decldebug.stseg
# C_CXX_FLAGS=-O0 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -DDEBUG -g -DPZ_DEV

# Enable static linking
# MCFLAGS=--mercury-linkage static
# C_CXX_FLAGS=-O2 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -Wno-error

# Enable optimisation,
# Remember to comment-out the development build options above.
# MCFLAGS=-O4 --intermodule-optimisation
# C_CXX_FLAGS=-O3 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -Wno-error

# Enable both static linking and optimisation
# Remember to comment-out the development build options above.
# MCFLAGS=-O4 --intermodule-optimisation \
#   --mercury-linkage static
# C_CXX_FLAGS=-O3 -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -Wno-error

# Enable Mercury profiling
# MCFLAGS=--grade asm_fast.gc.profdeep.stseg

# Extra features
# --------------
#
# These can be uncommented to add extra features of interest to developers.

# Tracing of the type checking/inference solver.
# MCFLAGS+=--trace-flag typecheck_solve

