/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_2

entrypoint
func main() uses IO -> Int {
    func test(beer : func(Int) -> String) uses IO {
        print!(beer(10) ++ "\n")
        print!(beer(5) ++ "\n")
        print!(beer(1) ++ "\n")
        print!(beer(0) ++ "\n")
        print!(beer(-1) ++ "\n")

    }

    test!(beer1)
    test!(beer2)
    test!(beer3)
    test!(beer4)

    return 0
}

func beer1(n : Int) -> String {
    // This match expression returns two items.
    var beer_str, var panic = match (n) {
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
    var beer_str, var panic =
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

func beer3(n : Int) -> String {
    var beer_str, var panic =
        // Check that different arms of the if-then-else can can have
        // different expressions although the expression on the else branch
        // returns 2 items.
        if (n < 0) then
            "You owe someone a beer!",
            "Better repay them!"
        else beer3_aux(n)

    return beer_str ++ " " ++ panic
}

func beer4(n : Int) -> String {
    var beer_str, var panic =
        // Check that different arms of the if-then-else can can have
        // different expressions although the expression on the else branch
        // returns 2 items.
        match (n) {
            -1 -> "You owe someone a beer!",
                  "Better repay them!"
            _ ->  beer3_aux(n)
        }

    return beer_str ++ " " ++ panic
}

// This can work to return multiple values.
func beer3_aux(n : Int) -> (String, String) {
    return if (n == 0) then
            "No more beer!",
            "PANIC!"
        else if (n == 1) then
            "Only one beer left.",
            "worry..."
        else
            int_to_string(n) ++ " more beers left.",
            ""
}

