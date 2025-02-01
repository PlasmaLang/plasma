#/bin/sh

TOP=../..

$TOP/src/plzlnk -n Module_08a -e Module_08.name1 -o module_08a.pz \
    module_08.pzo
$TOP/runtime/plzrun module_08a.pz

