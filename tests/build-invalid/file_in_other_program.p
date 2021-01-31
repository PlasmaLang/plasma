/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module FileInOtherProgram

import OtherProgram

entrypoint
func main() uses IO -> Int {
    print!("Hello\n")
    return 0
}

