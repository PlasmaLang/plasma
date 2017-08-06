# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Arity

func main() -> Int using IO {
    foo!(int_to_string(bar(3, 5)) ++ "\n")

    do_pm!(7)
    do_pm!(-23)

    return 0
}

# Test a function that returns nothing.
func foo(x : String) using IO {
    print!(x)
}

# A function that returns one thing.
func bar(a : Int, b : Int) -> Int {
    return a + b
}

func do_pm(x : Int) using IO {
    p, m = pm(x)
    print!("p: " ++ int_to_string(p) ++ ", m: " ++ int_to_string(m) ++ "\n")
}

# A function that returns two things.
func pm(x : Int) -> Int, Int {
    if (x < 0) {
        x_abs = x * -1
    } else {
        x_abs = x
    }
    return x_abs, x_abs * -1
}

