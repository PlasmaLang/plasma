/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ImportFromTwoModules2 

pragma foreign_include("import_function.h")

func foo() uses IO
    foreign(foo)

export
func test() uses IO {
    print!("Doing another foreign call\n")
    foo!()
    print!("done\n")
}

