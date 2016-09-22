#!/bin/sh
#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

# TODO: Use portable colours or don't use colours when not supported.

set -e

NUM_TESTS=0
NUM_SUCCESSES=0
FAILURE=0
TESTS=""

for PZTFILE in pzt/*.pzt; do
    NAME=$(basename $PZTFILE .pzt)
    TESTS="$TESTS pzt/$NAME"
done

for PFILE in p/*.p; do
    NAME=$(basename $PFILE .p)
    TESTS="$TESTS p/$NAME"
done

for TEST in $TESTS; do
    echo -n $TEST..."\033[20G"
    NAME=$(basename $TEST .test)
    DIR=$(dirname $TEST)
    # Wrapping this up in a test and negating it is a bit annoying, but it
    # was the easy way I could redirect the output and errors successfully.
    if [ ! "$(cd $DIR ; make $NAME.test 2>&1 > $NAME.log)" ]; then
        echo "\033[1;32mok\033[0m"
        NUM_SUCCESSES=$(echo $NUM_SUCCESSES + 1 | bc)
    else
        echo "\033[1;31mFAILED\033[0m"
        FAILURE=1
    fi
    NUM_TESTS=$(echo $NUM_TESTS + 1 | bc)
done

if [ $FAILURE -eq 0 ]; then
    echo "\033[1;32mAll $NUM_TESTS tests passed\033[0m"
else
    NUM_FAILED=$(echo $NUM_TESTS - $NUM_SUCCESSES | bc)
    echo -n "$NUM_SUCCESSES out of $NUM_TESTS passed, "
    echo "\033[1;31m$NUM_FAILED failed\033[0m"
fi

return $FAILURE
