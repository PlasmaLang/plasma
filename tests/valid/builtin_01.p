/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module Builtin01

/*
 * This test should be kept up-to-date with the documentation for the
 * builtins in docs/plasma_ref.txt
 */
entrypoint
func main() uses IO -> Int {
    test_maybe!()

    test_misc!()

    return 0
}

/* ****************************************** */

func test_maybe() uses IO {
    var a = None
    var b = Some(23)
    func maybe_str(m : Maybe(String)) -> String {
        return match(m) {
            None -> "None"
            Some(var x) -> "Some(" ++ x ++ ")"
        }
    }

    print!("Map result: " ++
           maybe_str(maybe_map(int_to_string, maybe_map(plus1, a))) ++
           "\n") 
    print!("Map result: " ++
           maybe_str(maybe_map(int_to_string, maybe_map(plus1, b))) ++
           "\n")
}

func maybe_map(f : func('a) -> 'b, m : Maybe('a)) -> Maybe('b) {
    return match(m) {
        None -> None
        Some(var x) -> Some(f(x))
    }
}

func plus1(x : Int) -> Int {
    return x + 1
}

/* ****************************************** */

// We can name some resources
resource A from IO
resource B from Time
resource C from Environment


func test_misc() uses IO {
    print!("Print works, duh\n")

    // Don't actually do it because the test doesn't read any standard
    // input.  We have other tests for that.
    if (False) {
        _ = readline!()
    } else {}

    _ = Builtin.set_parameter!("nothing", 2)
    _, _ = Builtin.get_parameter!("heap_usage")

    func do_setenv() uses Environment -> Bool {
        // Wrap in this function to test that it uses the right resource.
        return setenv!("Foo", "Bar")
    }
    var r = do_setenv!()
    print!("setenv result: " ++ bool_to_string(r) ++ "\n")
   
    func do_gettimeofday() uses Time {
        _, _, _ = Builtin.gettimeofday!()
    }
    do_gettimeofday!()

    if (False) {
        Builtin.die!("Die!")
    } else {}
}



