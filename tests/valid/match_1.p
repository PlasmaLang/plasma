/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Match_1

export main

import io

func main() uses IO -> Int {
    print!("fib1(16) = " ++ int_to_string(fib1(16)) ++ "\n")
    print!("fib2(16) = " ++ int_to_string(fib2(16)) ++ "\n")
    print!("fib3(16) = " ++ int_to_string(fib3(16)) ++ "\n")
    print!("fib4(16) = " ++ int_to_string(fib4(16)) ++ "\n")
    test5!()
    return 0
}

func fib1(n : Int) -> Int {
    match (n) {
        0 -> {
            return 1
        }
        1 -> {
            return 1
        }
        m -> {
            return fib1(m-1) + fib1(m-2)
        }
    }
}

func fib2(n : Int) -> Int {
    var r
    match (n) {
        0 -> {
            r = 1
        }
        1 -> {
            r = 1
        }
        m -> {
            r = fib2(m-1) + fib2(m-2)
        }
    }

    return r
}

func fib3(n : Int) -> Int {
    var r
    match (n) {
        0 -> {
            r = 1
        }
        1 -> {
            m = 1
            r = m
        }
        m -> {
            r = fib3(m-1) + fib3(m-2)
        }
    }

    return r
}

func fib4(n : Int) -> Int {
    var r
    match (n) {
        0 -> {
            r = 1
        }
        1 -> {
            m = "fish"
            r = 1
        }
        m -> {
            r = fib4(m-1) + fib4(m-2)
        }
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
    var beer_str, panic
    match (n) {
        -1 -> {
            beer_str = "You owe someone a beer!"
            panic = "Better repay them!"
        }
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

