/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Occurs5

type Occurs1('x) = Occurs1 ( v : 'x )
type Occurs2('x) = Occurs2 ( v : 'x )
type Occurs3('x) = Occurs3 ( v : 'x )

func occurs5(a : Occurs1(Occurs2(Occurs3('o))), b : 'o, c : Bool) uses IO {
    var r
    // This doesn't seem to be failing due to the occurs check, but it does
    // cause a type error so at least the compiler won't accept this invalid
    // program.
    if (c) {
        r = a
    } else {
        r = b
    }
    sink!(r)
}

func sink(a : 'a) uses IO { }

