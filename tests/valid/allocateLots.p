# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module AllocateLots

export main

import io

func main() uses IO -> Int {
    set_result = set_parameter!("heap_size", 4096)
    if (set_result) {
        allocate_lots_1(128)
        print!("Test completed\n")
        return 0
    } else {
        print!("Could not set heap_size parameter\n")
        return 1
    }
}

type MyCell = MyCell ( a : Int, b : Int )

func allocate_lots_1(n : Int) {
    if (n != 0) {
        cell = MyCell(n + n, n * n)
        allocate_lots_2(32)
        allocate_lots_1(n - 1)
    } else {
    }
}

func allocate_lots_2(n : Int) {
    if (n != 0) {
        cell = MyCell(n + n, n * n)
        allocate_lots_2(n - 1)
    } else {
    }
}

