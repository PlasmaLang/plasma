/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module MatchBadError1

export
func main() uses IO -> Int {
    test7!()
    return 0
}

type TypeExists = varctorname

func test7() uses IO {
    func t(a : Int) -> String {
        // This generates an error from the compiler, it's the wrong error
        // since it's a constructor that's already defined, but maybe it
        // should be a warning when a *local variable* does this to another
        // symbol?
        var varctorname
        var str
        match (a) {
            0 -> {
                varctorname = 0
                str = "zero"
            }
            // Same as test6 except this name is also a constructor name,
            // but eh compiler will choose the type.
            varctorname -> {
                str = "more"
            }
        }

        return int_to_string(varctorname) ++ " is " ++ str ++ "\n"
    }

    print!(t(0))
    print!(t(5))
}

