/*
 * vim: ft=plasma
 * This is free and unencumbered software released into the public domain.
 * See ../LICENSE.unlicense
 */

module ImportSharedModule 

pragma foreign_include("import_shared_module.h")

func test_extra()
    foreign(test_extra)

entrypoint
func test() uses IO -> Int {
    test_extra!()

    return 0
}

