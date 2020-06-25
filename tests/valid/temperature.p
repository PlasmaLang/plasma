/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Temperature

export
func main() uses IO -> Int {
    print!("0c is " ++ int_to_string(c_to_f(0)) ++ "f\n")
    print!("26c is " ++ int_to_string(c_to_f(26)) ++ "f\n")
    print!("37c is " ++ int_to_string(c_to_f(37)) ++ "f\n")
    print!("38c is " ++ int_to_string(c_to_f(38)) ++ "f\n")
    print!("100c is " ++ int_to_string(c_to_f(100)) ++ "f\n")

    return 0
}

func c_to_f(c : Int) -> Int {
    return c * 9 / 5 + 32
}

