#!/bin/sh

TOP=../..

$TOP/src/plzlnk -n Module_03a -e Module_03a.main -o module_03a.pz \
    module_03b.pzo module_03a.pzo
$TOP/runtime/plzrun module_03a.pz

