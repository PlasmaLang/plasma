/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Ite_3 

entrypoint
func main() uses IO -> Int {
    print!("fib1(16) = " ++ int_to_string(fib1(16)) ++ "\n")
    print!("fib2(16) = " ++ int_to_string(fib2(16)) ++ "\n")
    print!("fib4(16) = " ++ int_to_string(fib4(16)) ++ "\n")
    test5!()
    return 0
}

func fib1(n : Int) -> Int {
    if (n <= 1) {
        return 1
    } else {
        return fib1(n-1) + fib1(n-2)
    }
}

func fib2(n : Int) -> Int {
    var r
    if (n <= 1) {
        r = 1
    } else {
        r = fib2(n-1) + fib2(n-2)
    }

    return r
}

func fib4(n : Int) -> Int {
    var r
    if (n <= 1) {
        var m = "fish"
        r = 1
    } else {
        var m = n
        r = fib4(m-1) + fib4(m-2)
    }

    return r
}

func test5() uses IO {
    print!(beer(10) ++ "\n")
    print!(beer(5) ++ "\n")
    print!(beer(1) ++ "\n")
    print!(beer(0) ++ "\n")
    print!(beer(-1) ++ "\n")
}

/* 
 * Test switches that provide multiple values
 * Test wildcard matches
 * Test negative patterns
 */
func beer(n : Int) -> String {
    // This could be an expression but those are tested in match_2.p
    var beer_str
    var panic
    if (n < 0) {
        beer_str = "You owe someone a beer!"
        panic = "Better repay them!"
    } else if (n == 0) {
        beer_str = "No more beer!"
        panic = "PANIC!"
    } else if (n == 1) {
        beer_str = "Only one beer left."
        panic = "worry..."
    } else {
        beer_str = int_to_string(n) ++ " more beers left."
        panic = ""
    }

    return beer_str ++ " " ++ panic
}

