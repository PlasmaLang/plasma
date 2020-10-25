/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Module_09a

export
func getMessage() -> String {
    return "universe!"
}

export
func printMessage(message : String) uses IO {
    print!(message ++ "\n")
}

