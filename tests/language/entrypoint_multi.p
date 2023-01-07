/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module EntrypointMulti 

entrypoint
func name1() uses IO -> Int {
    print!("Hello world 1\n") 
    return 0
}

entrypoint
func name2() uses IO -> Int {
    print!("Hello world 2\n") 
    return 0
}

