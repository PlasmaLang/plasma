/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ArityLambda

export
func main() uses IO -> Int {
    func test() -> Int {
        // Error, there's no return statment
    }

    return 0
}

