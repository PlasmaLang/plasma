/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module HO_7

export main
import io

func main() uses IO -> Int {
    // print_one uses a resource that do_for will not make available.
    do_for2!(print_one, [1, 2, 3])
    print!("\n")

    // Put a higher order thing in a structure, then use it but without the
    // correct resource.
    x = MyType(print)
    apply!(x, "Hi\n")

    return 0
}

////////

func test() {
    // Basic HO use.
    // These currently generate confusing error messages, but it's still
    // something we can test.
    x = hello_msg
    x!("Paul")
}

func hello_msg(name : String) uses IO {
    print!("Hello " ++ name ++ "\n")
}

////////

func do_for1(f : func(x) uses IO, l : List(x)) uses IO {
    match (l) {
        [] -> {}
        [x | xs] -> {
            // Missing bang.
            f(x)
            do_for1!(f, xs)
        }
    }
}

func do_for2(f : func(x), l : List(x)) uses IO {
    match (l) {
        [] -> {}
        [x | xs] -> {
            // f doesn't use a resource.
            f!(x)
            do_for2!(f, xs)
        }
    }
}

func print_one(n : Int) uses IO {
    print!(int_to_string(n) ++ ", ")
}

////////

type MyType(x) = MyType(x : x)

func apply(mt : MyType(func(x)), x : x) uses IO {
    match(mt) {
        MyType(f) -> { f(x) }
    }
}

func apply2(mt : MyType(func(x) uses IO), x : x) uses IO {
    match(mt) {
        // Call to f should have a !.
        MyType(f) -> { f(x) }
    }
}

//////

resource A from IO
resource B from IO

func test_return() uses A {
    // Return a resource using function from a function and call it.
    f = get_func(Colour)
    f!("Blue")
}

// TODO: This example would be more idiomatic if we supported currying or lambdas
type FavouriteThing = Colour
                    | Season

func favourite_colour(c : String) uses B {
    // print!("My favorite colour is " ++ c ++ "\n")
}
func favourite_season(s : String) uses B {
    // print!("My favorite season is " ++ s ++ "\n")
}

func get_func(thing : FavouriteThing) -> func(String) uses B {
    match(thing) {
        Colour -> { return favourite_colour }
        Season -> { return favourite_season }
    }
}

func get_func_broke(thing : FavouriteThing) -> func(String) uses A {
    match(thing) {
        Colour -> { return favourite_colour }
        Season -> { return favourite_season }
    }
}

//////
