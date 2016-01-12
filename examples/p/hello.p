# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../../LICENSE.unlicense

# Hello module declaration, this gives the name of the module.
module Hello

# An export list each symbol named by an export list is exported from the
# module.
export main

# Import modules that we'll need.
import io
import list
import string

# The main function, returns int, takes a single argument, argv, which is
# list of strings.  The main function uses the IO resource.
func main(argv : List(String)) -> Int using IO {
    # the ! indicates that this statement uses a resource, which one is
    # determined automatically where possible.
    ! print("Hello world\n")

    # The value of a function (or block) is the value of its last
    # statement.
    # XXX EXIT_SUCCESS
    0
}

