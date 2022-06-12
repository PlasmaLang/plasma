/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Operators

/*
 * Please keep this up to date with the documentation in docs/plasma_ref.txt
 */

entrypoint
func main() uses IO -> Int {
    // Arithmetic
    print!("27 + 3 = " ++ int_to_string(27 + 3) ++ "\n")
    print!("27 - 3 = " ++ int_to_string(27 - 3) ++ "\n")
    print!("3 - 27 = " ++ int_to_string(3 - 27) ++ "\n")
    print!("5 * 5 = " ++ int_to_string(5 * 5) ++ "\n")
    print!("37 / 5 = " ++ int_to_string(37 / 5) ++ "\n")
    print!("-37 / 5 = " ++ int_to_string(-37 / 5) ++ "\n")
    print!("37 / -5 = " ++ int_to_string(37 / -5) ++ "\n")
    print!("-37 / -5 = " ++ int_to_string(-37 / -5) ++ "\n")
    print!("37 % 5 = " ++ int_to_string(37 % 5) ++ "\n")
    print!("-37 % 5 = " ++ int_to_string(-37 % 5) ++ "\n")
    print!("37 % -5 = " ++ int_to_string(37 % -5) ++ "\n")
    print!("-37 % -5 = " ++ int_to_string(-37 % -5) ++ "\n")
    print!("-3 = " ++ int_to_string(-3) ++ "\n")

    // Order of operations
    print!("12 + 3 * 4 = " ++ int_to_string(12 + 3 * 4) ++ "\n")
    print!("12 - (8 + 2) = " ++ int_to_string(12 - (8 + 2)) ++ "\n")
    print!("6 + 6 == 3 * 4 = " ++ bool_to_string(6 + 6 == 3 * 4) ++ "\n")

    // Comparison
    func test_compare(x : Int) uses IO {
        print!("5 < " ++ int_to_string(x) ++ " = " ++ 
            bool_to_string(5 < x) ++ "\n")
        print!("5 > " ++ int_to_string(x) ++ " = " ++ 
            bool_to_string(5 > x) ++ "\n")
        print!("5 <= " ++ int_to_string(x) ++ " = " ++ 
            bool_to_string(5 <= x) ++ "\n")
        print!("5 >= " ++ int_to_string(x) ++ " = " ++ 
            bool_to_string(5 >= x) ++ "\n")
        print!("5 == " ++ int_to_string(x) ++ " = " ++ 
            bool_to_string(5 == x) ++ "\n")
        print!("5 != " ++ int_to_string(x) ++ " = " ++ 
            bool_to_string(5 != x) ++ "\n")
    }
    var list = [3, 4, 5, 6, 7]
    do_list!(test_compare, list)

    // Bool
    print!("not True = " ++ bool_to_string(not True) ++ "\n")
    print!("not False = " ++ bool_to_string(not False) ++ "\n")
    func test_and(b1 : Bool, b2 : Bool) uses IO {
        print!(bool_to_string(b1) ++ " and " ++ bool_to_string(b2) ++ " = "
            ++ bool_to_string(b1 and b2) ++ "\n")
    }
    test_and!(False, False)
    test_and!(False, True)
    test_and!(True, False)
    test_and!(True, True)
    func test_or(b1 : Bool, b2 : Bool) uses IO {
        print!(bool_to_string(b1) ++ " or " ++ bool_to_string(b2) ++ " = "
            ++ bool_to_string(b1 or b2) ++ "\n")
    }
    test_or!(False, False)
    test_or!(False, True)
    test_or!(True, False)
    test_or!(True, True)

    // String append is tested throughout this test anyway.

    return 0
}

func do_list(f : func('x) uses IO, l : List('x)) uses IO {
    match (l) {
        [] -> { }
        [var x | var xs] -> {
            f!(x)
            do_list!(f, xs)
        }
    }
}

