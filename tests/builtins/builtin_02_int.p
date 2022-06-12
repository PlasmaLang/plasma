/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Builtin02Int

/*
 * This test should be kept up-to-date with the documentation for the
 * builtins in docs/plasma_ref.txt
 */
entrypoint
func main() uses IO -> Int {
    // Tests for integer oeprators are in operators.p

    // We can name the Int type.
    func foo(v : Int) -> Int {
        return v * 42
    }

    print!("int_add(5, 3) = " ++
        int_to_string(Builtin.int_add(5, 3)) ++ "\n")
    print!("int_sub(5, 3) = " ++
        int_to_string(Builtin.int_sub(5, 3)) ++ "\n")
    print!("int_mul(5, 3) = " ++
        int_to_string(Builtin.int_mul(5, 3)) ++ "\n")
    print!("int_div(36, 7) = " ++
        int_to_string(Builtin.int_div(36, 7)) ++ "\n")
    print!("int_mod(36, 7) = " ++
        int_to_string(Builtin.int_mod(36, 7)) ++ "\n")
    print!("int_minus(23) = " ++
        int_to_string(Builtin.int_minus(23)) ++ "\n")

    // Builtin Int functions that are not operators, including
    // int_to_string. 
    print!("int_leftshift(5, 3) = " ++
        int_to_string(Builtin.int_lshift(5, 3)) ++ "\n")
    print!("int_rightshift(37, 2) = " ++
        int_to_string(Builtin.int_rshift(37, 2)) ++ "\n")
    print!("int_and(15, 28) = " ++
        int_to_string(Builtin.int_and(15, 28)) ++ "\n")
    print!("int_or(15, 28) = " ++
        int_to_string(Builtin.int_or(15, 28)) ++ "\n")
    print!("int_xor(15, 28) = " ++
        int_to_string(Builtin.int_xor(15, 28)) ++ "\n")
    print!("int_comp(5) = " ++ 
        int_to_string(Builtin.int_comp(5)) ++ "\n")

    print!("int_gt(3, 5) = " ++
        bool_to_string(Builtin.int_gt(3, 5)) ++ "\n")
    print!("int_lt(3, 5) = " ++
        bool_to_string(Builtin.int_lt(3, 5)) ++ "\n")
    print!("int_gteq(3, 5) = " ++
        bool_to_string(Builtin.int_gteq(3, 5)) ++ "\n")
    print!("int_lteq(3, 5) = " ++
        bool_to_string(Builtin.int_lteq(3, 5)) ++ "\n")
    print!("int_eq(3, 5) = " ++
        bool_to_string(Builtin.int_eq(3, 5)) ++ "\n")
    print!("int_neq(3, 5) = " ++
        bool_to_string(Builtin.int_neq(3, 5)) ++ "\n")

    return 0
}

