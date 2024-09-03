/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module PragmaUnknown1

pragma sillyname("skidoo")

entrypoint func main() uses IO -> Int {
    print!("Hello world!\n")
    return 0
}

