# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# Hello module declaration, this gives the name of the module.
module Match_1 

# An export list each symbol named by an export list is exported from the
# module.
export main

# Import modules that we'll need.
import io

func main() -> Int using IO {
    x = 3
    match (x) {
        3 -> {
            z = 4
        }
        yy -> {
            y = yy * 26
        }
    }

    print!(int_to_string(y))
    return 0
}

