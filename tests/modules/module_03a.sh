#!/bin/sh

TOP=../..

# Link two modules, both with entry points and check that we get the correct
# one.
#
# Test b chooses the "other" entrypoint,
# Test ar and br link the modules in the reverse order.
# Test c links only one module with an entry point and checks that it is
# selected implicity. 

$TOP/src/plzlnk -n Module_03a -e Module_03a.main -o module_03a.pz \
    module_03a.pzo module_03b.pzo
$TOP/runtime/plzrun module_03a.pz

