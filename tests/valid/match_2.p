/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_2

export
func main() uses IO -> Int {
    print!(beer(10) ++ "\n")
    print!(beer(5) ++ "\n")
    print!(beer(1) ++ "\n")
    print!(beer(0) ++ "\n")
    print!(beer(-1) ++ "\n")
    return 0
}

func beer(n : Int) -> String {
    // This match expression returns two items.
    var beer_str, panic
    beer_str, panic = match (n) {
            -1 -> "You owe someone a beer!",
                  "Better repay them!"
            0 -> "No more beer!",
                 "PANIC!"
            1 -> "Only one beer left.",
                 "worry..."
            _ -> int_to_string(n) ++ " more beers left.",
                 ""
        }

    return beer_str ++ " " ++ panic
}

