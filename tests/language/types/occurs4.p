/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Occurs4

type Occurs('x) = Occurs ( v : 'x ) | Nil

func occurs4() uses IO{
    var o1 = faucet()
    match (o1) {
        Occurs(var o2) -> {
            if (eq(o1, o2)) {
                print!("True")
            } else {
                print!("False")
            }
        }
        Nil -> { Builtin.die!("Die!") }
    }
}

func eq(x : 'a, y : 'a) -> Bool {
    return True
}

func faucet() -> Occurs('o) { 
    return Nil
}

/*
 * We should also test the occurs check on function types, but the test can't
 * be described in Plasma yet without more functional features.
 */

