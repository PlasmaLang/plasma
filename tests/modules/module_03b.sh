#!/bin/sh

TOP=../..

$TOP/src/plzlnk -n Module_03b -e Module_03b.main -o module_03b.pz \
    module_03a.pzo module_03b.pzo
$TOP/runtime/plzrun module_03b.pz

