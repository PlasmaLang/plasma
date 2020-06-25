/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module AllocateLots

export
func main() uses IO -> Int {
    print_heap_size!()
    var collections_start = heap_collections!()

    var tree = foldl(insert_wrapper, big_list(), Empty)
    traverse!(print_node, tree)
    print_heap_size!()
    var collections_end = heap_collections!()
    if (collections_end <= collections_start) {
        print!("Allocate lots did not GC\n")
        return 1
    } else {
        print!("# There were " ++
            int_to_string(collections_end - collections_start) ++
            " collections during the test.\n")
        return 0
    }
}

func heap_collections() uses IO -> Int {
    var res, collections = get_parameter!("heap_collections")
    if (res) {
        print!("# There have been " ++ int_to_string(collections) ++
            " GCs.\n")
    } else {
        die("Can't retrive heap_collections\n")
    }
    return collections
}

func print_heap_size() uses IO {
    var res, heap_size = get_parameter!("heap_usage")
    if (res) {
        print!("# Heap_size: " ++ int_to_string(heap_size/1024) ++ "KB\n")
    } else {
    }
}

func foldl(f : func('x, 'a) -> 'a, l : List('x), a0 : 'a) -> 'a {
    match (l) {
        [] -> {
            return a0
        }
        [var x | var xs] -> {
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

type Tree('k, 'v) = Empty
                  | Tree(
                      left    : Tree('k, 'v),
                      key     : 'k,
                      value   : 'v,
                      right   : Tree('k, 'v)
                  )

func insert(compare : func('k, 'k) -> Int, tree : Tree('k, 'v),
            key : 'k, value : 'v)
        -> Tree('k, 'v)
{
    match (tree) {
        Empty -> { return Tree(Empty, key, value, Empty) }
        Tree(var left, var tkey, var tvalue, var right) -> {
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

func traverse(f : func('k, 'v) uses IO, tree : Tree('k, 'v)) uses IO {
    match (tree) {
        Empty -> {}
        Tree(var left, var key, var value, var right) -> {
            traverse!(f, left)
            f!(key, value)
            traverse!(f, right)
        }
    }
}

func big_list() -> List(Int) {
    return [136, 294, 197, 215, 192, 127, 105, 212, 48, 161, 209, 119, 71,
            141, 165, 291, 181, 169, 221, 56, 280, 222, 29, 267, 235, 140,
            54, 157, 80, 37, 234, 242, 12, 53, 92, 194, 102, 200, 43, 179,
            51, 44, 166, 177, 173, 150, 42, 198, 31, 104, 162, 205, 229,
            286, 213, 262, 281, 261, 133, 189, 112, 257, 9, 18, 100, 204,
            75, 57, 299, 28, 269, 47, 138, 41, 66, 25, 288, 109, 185, 130,
            49, 193, 147, 285, 292, 207, 196, 245, 111, 239, 240, 260, 106,
            86, 137, 70, 271, 247, 160, 52, 8, 259, 190, 217, 45, 21, 23,
            91, 79, 117, 0, 270, 236, 99, 59, 223, 295, 64, 206, 38, 3, 224,
            128, 220, 231, 101, 171, 125, 1, 90, 254, 17, 34, 230, 120, 110,
            30, 210, 39, 11, 67, 232, 84, 186, 156, 24, 20, 187, 93, 19,
            163, 266, 108, 132, 195, 129, 116, 146, 178, 69, 33, 26, 290,
            250, 144, 131, 233, 263, 2, 50, 73, 134, 175, 226, 168, 248,
            297, 60, 228, 225, 107, 145, 237, 55, 65, 96, 279, 155, 287, 6,
            256, 296, 182, 293, 202, 46, 152, 118, 265, 201, 218, 149, 15,
            61, 208, 95, 277, 219, 273, 275, 298, 227, 72, 68, 252, 268,
            167, 40, 143, 97, 124, 284, 77, 191, 83, 164, 13, 78, 114, 282,
            126, 244, 148, 58, 278, 238, 82, 115, 113, 211, 98, 289, 151,
            135, 89, 243, 153, 216, 251, 74, 184, 246, 214, 122, 174, 94, 7,
            22, 253, 87, 81, 183, 283, 188, 16, 10, 203, 241, 264, 274, 27,
            159, 4, 276, 88, 32, 158, 154, 139, 14, 255, 199, 5, 121, 272,
            258, 176, 170, 103, 172, 142, 85, 35, 36, 76, 249, 63, 62, 123,
            180]
}

