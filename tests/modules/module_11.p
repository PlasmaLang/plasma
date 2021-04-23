/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_11

import Module_11.B as B

// Remove this once we make transitive resource names available.
import Module_11.A as A

entrypoint
func main() uses IO -> Int {
    print!("Test!\n")
  
    var r = do2!() 
    print!("The meaning of life: " ++ int_to_string(r) ++ "\n")
    
    return 0
}

func do2() uses B.Res2 -> Int {
    return do4!() * 2
}

func do4() uses B.Res4 -> Int {
    return 21
}

