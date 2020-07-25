/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_02a

import Module_02

export
func is_even(n : Int) -> Bool {
    if (n == 0) {
        return True
    } else {
        return Module_02.is_odd(n - 1)
    }
}


