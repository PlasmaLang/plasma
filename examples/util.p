/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Util

export
func while(f : func() uses IO -> Bool) uses IO {
    var res = f!()
    if (res) {
        while!(f)
    } else {
    }
}

export
func do_for(f : func('x) uses IO, l : List('x)) uses IO {
    match (l) {
        [] -> {}
        [var x | var xs] -> {
            f!(x)
            do_for!(f, xs)
        }
    }
}

