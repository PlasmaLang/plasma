/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Builtin03Bool

/*
 * This test should be kept up-to-date with the documentation for the
 * builtins in docs/plasma_ref.txt
 */
entrypoint
func main() uses IO -> Int {

    // Show that we can name the type and constructor.
    func do_a_bool(a : Bool) -> Bool {
        return not a and True
    }
    print!("test: " ++ bool_to_string(do_a_bool(True)) ++ " and " ++
        bool_to_string(do_a_bool(False)) ++ "\n")

    // Let's use some builtin functions, but as higher-order values.
    func do_test(name : String, f : func(Bool, Bool) -> Bool) uses IO {
        print!(name ++ ": " ++ bool_to_string(f(True, False)) ++ "\n")
    }
    do_test!("and", Builtin.bool_and)
    do_test!("or", Builtin.bool_or)
    print!("not: " ++ bool_to_string(Builtin.bool_not(True)) ++ "\n")
 
    return 0
}

