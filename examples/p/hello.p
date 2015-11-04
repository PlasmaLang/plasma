# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../../LICENSE.unlicense

# Hello module declaration.  Within the brackets is the "export list".
# Anything not included in the export list cannot be used from another
# module.
module Hello {
    main
}

# Import modules that we'll need.
import io
import list
import string

# The main function, returns int, takes a single argument, argv, which is
# list of strings.  The main function uses the IO resource.
main(argv : list(string)) -> int using IO {
    # the ! indicates that this statement uses a resource, which one is
    # determined automatically where possible.
    ! print("Hello world\n")

    # The value of a function (or block) is the value of it's last
    # statement.
    EXIT_SUCCESS
}

