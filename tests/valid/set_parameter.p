# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

module SetParameter

export main

func main() uses IO -> Int {
    test_parameter!("heap_size", 0)
    test_parameter!("heap_size", 4096)
    test_parameter!("Squark!", 26)
    return 0
}

func test_parameter(name : String, value : Int) uses IO {
    res = set_parameter!(name, value)
    print!(pretty_result(res, name, value))
}

func pretty_result(res : Bool, label : String, value : Int) -> String {
    var res_str
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

