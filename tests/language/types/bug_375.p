/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Bug375

type C = C
type SP = SP

func sp_is_a(sp : SP) -> Bool { return False }

// A mistaken type here.
func sp_to_c(sp : SP) -> SP { return SP }

// Fixed
func sp_to_c_good(sp : SP) -> C { return C }

func find_last(test : func(C) -> Bool,
               string : String) {
    func loop(pos : SP) -> SP {
        if sp_is_a(pos) {
            return pos
            // Is not detected as an error here.
        } else if test(sp_to_c(pos)) {
            return pos
        } else {
            return pos
        }
    }
}

func find_last2(test : func(C) -> Bool,
               string : String) {
    func loop(pos : SP) -> SP {
        // This version does catch the error.
        if test(sp_to_c(pos)) {
            return pos
        } else {
            return pos
        }
    }
}

func find_last3(test : func(C) -> Bool,
               string : String) {
    func loop(pos : SP) -> SP {
        // This version has no error.
        if test(sp_to_c_good(pos)) {
            return pos
        } else {
            return pos
        }
    }
}
