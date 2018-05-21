# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module SetParameter

export main

func main() uses IO -> Int {
    res1 = set_parameter!("heap_size", 0)
    print!(pretty_result(res1, "heap_size", 0))

    res2 = set_parameter!("heap_size", 4096)
    print!(pretty_result(res1, "heap_size", 4096))

    return 0
}

func pretty_result(res : Bool, label : String, value : Int) -> String {
    match (res) {
        True -> {
            res_str = "Succeeded"
        }
        False -> {
            res_str = "Failed"
        }
    }
    return res_str ++ " to set " ++ label ++
        " to " ++ int_to_string(value) ++ "\n"
}

