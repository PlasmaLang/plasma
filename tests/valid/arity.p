/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Arity

func main() uses IO -> Int {
    foo!(int_to_string(bar(3, 5)) ++ "\n")

    do_pm!(7)
    do_pm!(-23)

    x = 3 // Check that the stack is still aligned.
    foo2!("Test foo2\n")
    foo3!("Test foo3\n")
    noop!()

    foo4!(4)

    // Multi-arity in higher-order code.
    n = 8
    f = fst(pm, n)
    s = snd(pm, n)

    print!("pm(" ++ int_to_string(n) ++ ") -> " ++
        int_to_string(f) ++ ", " ++ int_to_string(s) ++ "\n")

    return x - 3 + f + s
}

// Test a function that returns nothing.
func foo(x : String) uses IO {
    print!(x)
}

// This function returns numthing, but ends in an assignment, which is stupid
// but for now legal.  It should generate a warning in the future.
func foo2(x : String) uses IO {
    print!(x)
    y = x
}

// Test a function that returns nothing, and has an empty return statement.
func foo3(x : String) uses IO {
    print!(x)
    return
}

func noop() uses IO {}

// A function that returns one thing.
func bar(a : Int, b : Int) -> Int {
    return a + b
}

func do_pm(x : Int) uses IO {
    p, m = pm(x)
    print!("p: " ++ int_to_string(p) ++ ", m: " ++ int_to_string(m) ++ "\n")
}

func fst(f : func(Int) -> (Int, Int), input : Int) -> Int {
    a, _ = f(input)
    return a
}

func snd(f : func(Int) -> (Int, Int), input : Int) -> Int {
    _, b = f(input)
    return b
}

// A function that returns two things.
func pm(x : Int) -> (Int, Int) {
    if (x < 0) {
        x_abs = x * -1
    } else {
        x_abs = x
    }
    return x_abs, x_abs * -1
}

// Something that returns something may have its result thrown away.
// Although this specific example should be a warning since the call to bar
// also has no affects, it would be optimised away.
func foo4(x : Int) uses IO {
    print!("foo4\n")
    _ = bar(x, 23)
}

