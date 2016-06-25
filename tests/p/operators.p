# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# The compiler cannot yet handle this example.

module Operators

export main

func main() -> Int using IO {
    print!("37 / 5 = " ++ int_to_string(37 / 5) ++ "\n")
    print!("37 % 5 = " ++ int_to_string(37 % 5) ++ "\n")
    print!("1 << 8 = " ++ int_to_string(1 << 8) ++ "\n")
    print!("21 >> 2 = " ++ int_to_string(21 >> 2) ++ "\n")

    return 0
}

