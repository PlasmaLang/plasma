/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ImportTwoModules1 

import ImportTwoModules2 as I2M2

pragma foreign_include("import_function.h")

func getpid() -> Int
    foreign(my_getpid)

entrypoint
func hello() uses IO -> Int {
    print!("Hello world\n")

    var pid = getpid!()
    print!("# My pid is " ++ int_to_string(pid) ++ "\n")

    var pid2 = getpid!()
    if (pid == pid2) {
        print!("My pid didn't change\n")
    } else {
        print!("My pid changed, that's weird\n")
    }

    I2M2.test!()

    return 0
}

