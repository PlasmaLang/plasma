/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Vars_2

export main

import io

func main() uses IO -> Int {
    test1!()
    test2!()
    test3!(3)

    return 0
}

func test1() uses IO {
    // You can introduce a variable
    var msg1
    // Then initialise it
    msg1 = "Hello "

    // Or introduce and initialise it
    msg2 = "test 1"

    print!(msg1 ++ msg2 ++ "\n")
}

func test2() uses IO {
    var a, b
    a = 3
    b = 5
    print!("Test 2: a + b is " ++ int_to_string(a + b) ++ "\n")
}

func test3(q : Int) uses IO {
    var x
    if (q < 4) {
        x = "less than"
    } else {
        x = "greater than or equal to"
    }
    print!("test 3: " ++ int_to_string(q) ++ " is " ++ x ++ " 4\n")
}

