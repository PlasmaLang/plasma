
/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Builtin05List

/*
 * This test should be kept up-to-date with the documentation for the
 * builtins in docs/plasma_ref.txt
 */
entrypoint
func main() uses IO -> Int {

    // Nil can be spelt.
    var nil1 = Builtin.list_nil()
    var nil2 = []

    // Cons can be spelt
    var list1 = [1, 2, 3]
    var list2 = Builtin.list_cons(4, Builtin.list_cons(5, [6]))

    // Constrain the types.
    var final_list = concat([list1, nil1, list2, nil2])

    print!("The final list is: " ++ 
        join(", ", map(int_to_string, final_list)) ++ "\n")

    return 0
}

func append(l1 : List('t), l2 : List('t)) -> List('t) {
    return match (l1) {
        [] -> l2
        [var x | var xs] -> [x | append(xs, l2)]
    }
}

func concat(l : List(List('t))) -> List('t) {
    return match (l) {
        [] -> []
        [var x | var xs] -> append(x, concat(xs)) 
    }
}

func join(j : String, l : List(String)) -> String {
    func join2(x : String, xs : List(String)) -> String {
        return match (xs) {
            [] -> x
            [var y | var ys] -> x ++ j ++ join2(y, ys)
        }
    }

    return match (l) {
        [] -> ""
        [var x | var xs] -> join2(x, xs)
    }
}

func map(f : func('x) -> 'y, l : List('x)) -> List('y) {
    return match (l) {
        [] -> []
        [var x | var xs] -> [f(x) | map(f, xs)]
    }
}

