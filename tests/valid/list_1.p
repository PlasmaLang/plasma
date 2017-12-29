# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module List_1

export main

import io

func main() -> Int uses IO {
    list1 = [1, 2, 3, 4, 5]
    print!(int_to_string(reduce(add, list1, 0)) ++ "\n")

    list2 = [200 | list1]
    print!(int_to_string(reduce(add, list2, 0)) ++ "\n")

    list3 = [4000, 200 | list1]
    print!(int_to_string(reduce(add, list3, 0)) ++ "\n")

    # list4 = [1..10]
    # print!(int_to_string(reduce(add, list4, 0)) ++ "\n")

    return 0
}

func reduce(f : func(a, a) -> (a), l : List(a), acc0 : a) -> a
{
    match (l) {
        [] -> { return acc0 }
        [x | xs] -> {
            acc = f(x, acc0)
            return reduce(f, xs, acc)
        }
    }
}

func add(a : Int, b : Int) -> Int { return a + b }


