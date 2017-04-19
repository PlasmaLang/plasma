# vim: ft=plasma
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense

# This example of lists, streams and arrays is not yet supported.

module Sequences

export main

import io

func main() -> Int using IO {
    print!("lists\n")
    list = [1, 2, 3, 4]
    list2 = [0 | list]
    # cons several items at once.
    list3 = [-2, -1, 0 | list]

    # for x in list2 {
    #     print!(show(x) ++ "\n")
    # }

    print!("arrays\n")
    array = [: 1, 2, 3, 4 :]
    # for x in array {
    #     ! print(show(x) ++ "\n")
    # }
    # An array can be subscripted (1-based).
    print!("The second element in the array is: " ++ show(array[2]) ++
        "\n")
    # including assignment (array must be unique)
    array[2] <= 23
    # And indexed from the end.
#   print!("the second last element is: " ++ show(array[-2]) ++ "\n")
    # BUT the - symbol in the subscript is special, it means "count from the
    # end" and not "minus".  The actual expression must be an unsigned
    # integer type.  This cannot be implemented with the current parser.

#   # Or sliced (the syntax is Expr? '..' Expr?
#   array2 = array[1..3]    # first two elements.
#   array3 = array[2..4]    # 2nd and 3rd elements from the middle of the
#                           # array
#   array4 = array[3..]     # 3rd element to the end.
#   array5 = array[..3]     # first two elements
#   # Minuses can be used to position from the end.
#   array6 = array[-3..]    # 3rd last to end elements
#   array7 = array[..-1]    # All but the last element
#   array8 = array[-3..-1]  # 3rd and 2nd last elements

    # ! print("streams\n")
    # A stream may be part of a producer/consumer parallelism, maybe being
    # produced by another thread.  Or it may be lazy, being produced by
    # evaluating thunks in this thread, or already evaluated.
    # Streams do not need a syntax for constants the way lists and arrays
    # do, since they are either created lazily or concurrently.  Likewise
    # streams are usually consumed with a loop.  So I haven't defined any
    # syntax for them yet.  However I'm considering using [- -] symbols for
    # streams, possibly with comprehensions.
    # TODO: some stream examples..

    # Lists, arrays and streams can all be concatenated with sequences of
    # the same time (eg lists and lists) using the ++ operator.
    list4 = list ++ list2

    # I believe I will use the { } brackets for dictionaries.

    # Another idea, maybe N..M is syntax for a sequence of N to M
    # inclusive.  It could either be polymorphic (returning the sequence of
    # the desired type) or be used within brackets eg for a list: [ 1..10 ].
    # The latter makes things clear but the former looks good in
    # comprehensions.  I will decide later.

    return 0
}

