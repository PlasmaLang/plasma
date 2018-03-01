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
FAILING_TESTS=""

for PZTFILE in pzt/*.pzt; do
    TESTS="$TESTS ${PZTFILE%.pzt}"
done

for DIR in valid invalid missing ../examples; do
    for PFILE in $DIR/*.exp; do
        TESTS="$TESTS ${PFILE%.exp}"
    done
done

for TEST in $TESTS; do
    NAME=$(basename $TEST .test)
    DIR=$(dirname $TEST)
    # Wrapping this up in a test and negating it is a bit annoying, but it
    # was the easy way I could redirect the output and errors successfully.
    if [ ! "$(cd $DIR ; make $NAME.test 2>&1 > $NAME.log)" ]; then
        echo -n "\033[1;32m.\033[0m"
        NUM_SUCCESSES=$(($NUM_SUCCESSES + 1))
    else
        echo -n "\033[1;31m*\033[0m"
        FAILURE=1
        FAILING_TESTS="$FAILING_TESTS $TEST"
    fi
    NUM_TESTS=$(($NUM_TESTS + 1))
done
printf '\n'

if [ $FAILURE -eq 0 ]; then
    echo "\033[1;32mAll $NUM_TESTS tests passed\033[0m"
else
    NUM_FAILED=$(( $NUM_TESTS - $NUM_SUCCESSES ))
    echo -n "$NUM_SUCCESSES out of $NUM_TESTS passed, "
    echo "\033[1;31m$NUM_FAILED failed\033[0m"

    echo "Failing tests: $FAILING_TESTS"
fi

return $FAILURE
