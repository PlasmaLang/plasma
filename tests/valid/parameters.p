/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Parameters

export main

func main() uses IO -> Int {
    test_parameter!("heap_max_size", 0)
    test_parameter!("heap_max_size", 128*4096)
    test_parameter!("Squark!", 26)
    return 0
}

func test_parameter(name : String, value : Int) uses IO {
    print!("TEST: " ++ name ++ ": " ++ int_to_string(value) ++ "\n")

    var res1, val1 = get_parameter!(name)
    print!(pretty_get_result(res1, name, val1))

    var res2 = set_parameter!(name, value)
    print!(pretty_set_result(res2, name, value))
    
    var res3, val3 = get_parameter!(name)
    print!(pretty_get_result(res1, name, val3))
}

func pretty_set_result(res : Bool, label : String, value : Int) -> String {
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

func pretty_get_result(res : Bool, label : String, value : Int) -> String {
    var res_str
    var maybe_value
    match (res) {
        True -> {
            res_str = "Succeeded"
            maybe_value = " to " ++ int_to_string(value)
        }
        False -> {
            res_str = "Failed"
            maybe_value = ""
        }
    }
    return res_str ++ " to get " ++ label ++ maybe_value ++ "\n"
}

