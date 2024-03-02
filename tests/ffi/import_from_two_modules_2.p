/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ImportFromTwoModules2 

pragma foreign_include("import_from_two_modules.h")

func bar() uses IO
    foreign(bar)

export
func test() uses IO {
    print!("Doing another foreign call\n")
    bar!()
    print!("done\n")
}

