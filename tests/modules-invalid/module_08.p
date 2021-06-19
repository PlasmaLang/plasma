/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_08

// Import C but not D, show that we can use both C's resources, even the
// one that depends on D.
import Module_08.C as C

func testCallsCRes3() uses IO {
    // Using a resource from Module D does not work.
    C.test1!()

    // Or even using a resource from module C that is "from" a resource in D
    C.test2!()
    my_test!()
}

func my_test() uses C.Res3 {
}

