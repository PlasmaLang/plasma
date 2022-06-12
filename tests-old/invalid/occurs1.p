/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Occurs1

type Occurs('x) = Occurs ( v : 'x )

func occurs1(a : Occurs('o), b : 'o) -> Bool {
    return a == b 
}

