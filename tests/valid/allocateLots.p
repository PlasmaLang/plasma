/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module AllocateLots

export main

import io

func main() uses IO -> Int {
    // Temporary heap size until we tune how the GC handles different cell
    // sizes.
    var set_result = set_parameter!("heap_max_size", 14*4096)
    if (set_result) {
        var l = [38, 23, 54, 75, 91, 34, 14, 93, 96, 15, 94, 53, 46, 40, 2, 5,
            98, 47, 35, 41, 84, 72, 36, 45, 95, 19, 92, 63, 39, 71, 27, 29, 88,
            4, 16, 87, 68, 76, 32, 10, 70, 25, 97, 57, 11, 51, 24, 22, 74, 37,
            50, 55, 42, 64, 12, 67, 56, 33, 48, 81, 43, 1, 66, 17, 78, 21, 60,
            0, 26, 61, 28, 6, 3, 8, 73, 69, 52, 85, 20, 7, 44, 31, 86, 99, 89,
            18, 49, 79, 77, 59, 82, 83, 30, 58, 62, 9, 65, 13, 90, 80]
        var tree = foldl(insert_wrapper, l, Empty)
        traverse!(print_node, tree)

        return 0
    } else {
        print!("Could not set heap_max_size parameter\n")
        return 1
    }
}

func foldl(f : func(x, a) -> a, l : List(x), a0 : a) -> a {
    match (l) {
        [] -> {
            return a0
        }
        [x | xs] -> {
            var a1 = f(x, a0)
            var a = foldl(f, xs, a1)
            return a
        }
    }
}

func insert_wrapper(x : Int, t : Tree(Int, String)) -> Tree(Int, String) {
    return insert(compare_num, t, x, "item " ++ int_to_string(x))
}

func compare_num(a : Int, b : Int) -> Int { return a - b }

func print_node(k : Int, v : String) uses IO {
    print!(v ++ "\n")
}

type Tree(k, v) = Empty
                | Tree(
                      left    : Tree(k, v),
                      key     : k,
                      value   : v,
                      right   : Tree(k, v)
                  )

func insert(compare : func(k, k) -> Int, tree : Tree(k, v), key : k, value : v)
        -> Tree(k, v)
{
    match (tree) {
        Empty -> { return Tree(Empty, key, value, Empty) }
        Tree(left, tkey, tvalue, right) -> {
            if (compare(key, tkey) < 0) {
                return Tree(
                    insert(compare, left, key, value),
                    tkey, tvalue, right)
            } else {
                return Tree(left, tkey, tvalue,
                    insert(compare, right, key, value))
            }
        }
    }
}

func traverse(f : func(k, v) uses IO, tree : Tree(k, v)) uses IO {
    match (tree) {
        Empty -> {}
        Tree(left, key, value, right) -> {
            traverse!(f, left)
            f!(key, value)
            traverse!(f, right)
        }
    }
}

