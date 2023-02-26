/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module MatchMultiple 

export
func main() uses IO -> Int {
    var list1 = [1, 2, 3, 4, 5]
    print!(int_to_string(reduce(add, list1)) ++ "\n")

    return 0
}

func reduce(f : func(Int, Int) -> (Int), l : List(Int)) -> Int
{
    match (l) {
        [] ->                       { return 0 }
        [var x] ->                  { return x }
        [var a, var b | var xs ] -> { return add(a, reduce(f, [b | xs])) }
    }
}

func add(a : Int, b : Int) -> Int { return a + b }

