/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_2

export
func main() uses IO -> Int {
    print!(beer1(10) ++ "\n")
    print!(beer2(5) ++ "\n")
    print!(beer1(1) ++ "\n")
    print!(beer2(0) ++ "\n")
    print!(beer1(-1) ++ "\n")
    return 0
}

func beer1(n : Int) -> String {
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

func beer2(n : Int) -> String {
    // This match expression returns two items.
    var beer_str, panic
    beer_str, panic =
        if (n < 0) then
            "You owe someone a beer!",
            "Better repay them!"
        else if (n == 1) then
            "Only one beer left.",
            "worry..."
        else if (n == 0) then
            "No more beer!",
            "PANIC!"
        else
            int_to_string(n) ++ " more beers left.",
            ""

    return beer_str ++ " " ++ panic
}

