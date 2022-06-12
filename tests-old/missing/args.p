/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Args 

entrypoint
func main(args : List(String)) uses IO -> Int {
    foldl!(say_hi, args)

    return 0
}

func say_hi(name : String) uses IO {
    print!("Hello " ++ name ++ "\n")
}

func foldl(f : func('t) uses IO, l : List('t)) uses IO {
    match (l) {
        [] -> {}
        [var x | var xs] -> {
            f!(x)
            foldl!(f, xs)
        }
    }
}

