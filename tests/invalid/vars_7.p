/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_7

export foo

func foo(a : Int, b : Int) -> Int {
    var c = a + b
    c = a * b
    return c
}

