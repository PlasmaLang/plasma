# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# The compiler cannot yet handle this example.

module Temperature

export main

import io

func main() -> Int using IO {
    c = 38
    f = c_to_f(c)

    ! print("26c is " ++ int_to_string(f) ++ "f\n")

    return 0
}

func c_to_f(c :: Int) -> Int {
    return c * 9 / 5 + 32
}

