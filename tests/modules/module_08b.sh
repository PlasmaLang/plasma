#/bin/sh

TOP=../..

$TOP/src/plzlnk -n Module_08a -e Module_08.name2 -o module_08b.pz \
    module_08.pzo
$TOP/runtime/plzrun module_08b.pz

