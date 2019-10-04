/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Parameters

export main

func main() uses IO -> Int {
    test_parameter!("Squark!", 26, Stable)
    test_parameter!("heap_usage", 100, Volatile)
    test_parameter!("heap_collections", 100, Volatile)
    return 0
}

type Volatile = Volatile
              | Stable

func test_parameter(name : String, value : Int, volatile : Volatile) uses IO {
    print!("TEST: " ++ name ++ ": " ++ int_to_string(value) ++ "\n")

    var res1, val1 = get_parameter!(name)
    print!(pretty_get_result(res1, name, val1, volatile))

    var res2 = set_parameter!(name, value)
    print!(pretty_set_result(res2, name, value))
    
    var res3, val3 = get_parameter!(name)
    print!(pretty_get_result(res1, name, val3, volatile))
}

func pretty_set_result(res : Bool, label : String, value : Int) -> String {
    var res_str
    match (res) {
        True -> { res_str = "Succeeded" }
        False -> { res_str = "Failed" }
    }

    return res_str ++ " to set " ++ label ++ " to " ++
        int_to_string(value) ++ "\n"
}

func pretty_get_result(res : Bool, label : String, value : Int,
        volatile : Volatile) -> String
{
    var res_str
    var maybe_value
    match (res) {
        True -> {
            res_str = "Succeeded"
            var maybe_hash
            match (volatile) {
                Volatile -> { maybe_hash = "# " }
                Stable -> { maybe_hash = "" }
            }
            maybe_value = ": " ++ maybe_hash ++ int_to_string(value)
        }
        False -> {
            res_str = "Failed"
            maybe_value = ""
        }
    }

    return res_str ++ " to get " ++ label ++ maybe_value ++ "\n"
}

