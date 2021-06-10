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

func testType1() -> A.TypeA1 {
    return A.StructA1(23) 
}

func testTypeA4() -> B.TypeA4 {
    return B.StructA4(A.StructA3(B.StructA2(testType1())))
}

func testTypeB4() -> B.TypeB4(Int) {
    return B.StructB4(A.StructB3(B.StructB2(A.StructB1(555))))
}


// Import C but not D, show that we can use both C's resources, even the
// one that depends on D.
import Module_11.C as C

func testCallsCRes3() uses IO {
    C.test!(testUsesCRes3)
}

// We can't convert into this resource because we can't see Res2 without
// another import.
func testUsesCRes3() uses C.Res3 {
}

