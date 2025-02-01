#!/bin/sh

TOP=../..

$TOP/src/plzlnk -n Module_03c -o module_03c.pz \
    module_03a.pzo module_03c.pzo
$TOP/runtime/plzrun module_03c.pz

