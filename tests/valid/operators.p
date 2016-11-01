# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Operators

export main

func main() -> Int using IO {
    print!("37 / 5 = " ++ int_to_string(37 / 5) ++ "\n")
    print!("37 % 5 = " ++ int_to_string(37 % 5) ++ "\n")
    print!("1 << 8 = " ++ int_to_string(1 << 8) ++ "\n")
    print!("21 >> 2 = " ++ int_to_string(21 >> 2) ++ "\n")
    print!("~7 = " ++ int_to_string(~7) ++ "\n")
    print!("-3 = " ++ int_to_string(-3) ++ "\n")

    print!("not True = " ++ bool_to_string(not True) ++ "\n")
    print!("True and False = " ++ bool_to_string(True and False) ++ "\n")
    print!("True or False = " ++ bool_to_string(True or False) ++ "\n")

    return 0
}

