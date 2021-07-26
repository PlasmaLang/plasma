/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

/*
 * These are not the final names of the bitwise operators.  They will
 * probably be renamed and placed in a module.
 */

module Bitwise

entrypoint
func main() uses IO -> Int {
    print!("1 << 8 = " ++ int_to_string(int_lshift(1, 8)) ++ "\n")
    print!("21 >> 2 = " ++ int_to_string(int_rshift(21, 2)) ++ "\n")
    print!("~7 = " ++ int_to_string(int_comp(7)) ++ "\n")

    return 0
}

