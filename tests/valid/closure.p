# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module Closure

export main

import io

func main() uses IO -> Int {
    func hi(name : String) uses IO {
        print!("Hello " ++ name ++ "\n")
    }

    hi!("Paul")

    return 0
}

