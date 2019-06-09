/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_1 

export main
import IO

// TODO:
//  Need to implement and test HO values in type arguments

func f1(a : Int) -> Int { return a + 1 }
func f2(a : Int) -> Int { return a * 2 }
func f3(a : Int) -> Int { return pow(a, 3) }

func main() uses IO -> Int {
    // Basic HO use.
    var x = hello_msg
    print!(x("Paul"))

    // Basic HO call
    print!(apply(hello_msg, "Paul again"))

    // Reduce a function over a list.
    print!(int_to_string(reduce(add, up_to(10), 0)) ++ "\n")

    // Store functions in data.
    var l = map(apply_to_12, [f1, f2, f3])
    // TODO: make this more abstract to deomonstrate more higher order code.
    print!(join(", ", map(int_to_string, l)) ++ "\n")

    // Return functions from other functions.
    var f = get_func(Colour)
    print!(f("Blue") ++ "\n")
    // Function application syntax.
    print!(get_func(Season)("Winter") ++ "\n")
    // Function application syntax as a statement.
    var fav_season = get_func(Season)("Snow time")
    print!(fav_season ++ "\n")

    return 0
}

func hello_msg(name : String) -> String {
    return "Hello " ++ name ++ "\n"
}

func apply(f : func(a) -> (b), arg : a) -> b {
    return f(arg)
}

func reduce(f : func(x, a) -> (a), l : List(x), a : a) -> a {
    match (l) {
        [] ->       { return a }
        [x | xs] -> { return f(x, reduce(f, xs, a)) }
    }
}

func map(f : func(x) -> (y), l : List(x)) -> List(y) {
    match (l) {
        [] ->       { return [] }
        [x | xs] -> { return [f(x) | map(f, xs)] }
    }
}

func apply_to_12(f : func(Int) -> (y)) -> y { return f(12) }

func join(j : String, l0 : List(String)) -> String {
    match (l0) {
        [] ->       { return "" }
        // TODO once supported, test a nested pattern match:
        // [x] ->      { return x }
        // [x, y | l] -> { return x ++ j ++ join(j, [y | l]) }

        // for now:
        [x | l] -> {
            match (l) {
                [] ->      { return x }
                [_ | _] -> { return x ++ j ++ join(j, l) }
            }
        }
    }
}

/*-----*/

func add(a : Int, b : Int) -> Int {
    return a + b
}

func pow(a : Int, b : Int) -> Int
{
    match b {
        0 -> { return 1 }
        1 -> { return a }
        n -> { return a * pow(a, n-1) }
    }
}

func up_to(a : Int) -> List(Int) {
    if (a == 0) {
        return []
    } else {
        return [a | up_to(a - 1)]
    }
}

/*-----*/

// TODO: This example would be more idiomatic if we supported currying or lambdas

type FavouriteThing = Colour
                    | Season

func favourite_colour(c : String) -> String {
    return "My favorite colours is " ++ c
}
func favourite_season(s : String) -> String {
    return "My favorite season is " ++ s
}

func get_func(thing : FavouriteThing) -> func(String) -> String {
    match(thing) {
        Colour -> { return favourite_colour }
        Season -> { return favourite_season }
    }
}

