/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

// The filename is module_example, note that they don't have to match by case
// or underscores.
module ModuleToImport


// Resources may be exported
export
resource MyRes from IO

// Types may be exported (the constructors and fields are exported too)
export
type MyMaybe('x) = Nothing
                 | Some(x : 'x)

// Or opaque-exported (the constructors and fields are not exported)
export opaque
type Tree('k, 'v) = Empty 
                  | Node(
                        k : 'k,
                        v : 'v,
                        l : Tree('k, 'v),
                        r : Tree('k, 'v)
                    )

// Functions may be exported.
export
func test() uses IO {
    print!("Hello from ModuleToImport\n")
}

