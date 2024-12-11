/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ImportTwoSources 

pragma foreign_include("import_two_sources.h")
pragma foreign_include("import_shared_module.h")

func test_a()
    foreign(test_a)
func test_extra()
    foreign(test_extra)

entrypoint
func test() uses IO -> Int {
    test_a!()
    test_extra!()

    return 0
}

