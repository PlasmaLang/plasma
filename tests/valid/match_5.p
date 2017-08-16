# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Match_5

export main

import io

func main() -> Int uses IO {
    print!(beer(10) ++ "\n")
    print!(beer(5) ++ "\n")
    print!(beer(1) ++ "\n")
    print!(beer(0) ++ "\n")

    return 0
}

#
# Test switches that provide multiple values
# Test wildcard matches
#
func beer(n : Int) -> String {
    match (n) {
        0 -> {
            beer_str = "No more beer!"
            panic = "PANIC!"
        }
        1 -> {
            beer_str = "Only one beer left."
            panic = "worry..."
        }
        _ -> {
            beer_str = int_to_string(n) ++ " more beers left."
            panic = ""
        }
    }

    return beer_str ++ " " ++ panic
}

