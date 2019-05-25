/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Occurs3

type Occurs(x) = Occurs ( v : x )

func occurs3(a : Occurs(o), b : o, c : Bool) uses IO {
    if (c) {
        r = a
    } else {
        r = b
    }
    sink!(r)
}

func sink(o : Occurs(o)) uses IO { }

