#!/bin/sh
#
# This is free and unencumbered software released into the public domain.
# See ../LICENSE.unlicense
#
# vim: noet sw=4 ts=4
#

set -e

NUM_TESTS=0
NUM_SUCCESSES=0
FAILURE=0
TESTS=""
FAILING_TESTS=""
WORKING_DIR=$(pwd)

if [ 8 -le $(tput colors) ]; then
    TTY_TEST_SUCC=$(tput setaf 2)$(tput bold)
    TTY_TEST_FAIL=$(tput setaf 1)$(tput bold)
    TTY_RST=$(tput sgr0)
fi

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

    cd $DIR
    if make "$NAME.test" >"$NAME.log" 2>&1; then
        printf '%s.%s' "$TTY_TEST_SUCC" "$TTY_RST"
        NUM_SUCCESSES=$(($NUM_SUCCESSES + 1))
        case $DIR in
            pzt|valid)
                # Also run GC test
                if [ ! "$NAME" = "die" ]; then
                    if make "$NAME.gctest" > /dev/null 2>&1; then
                        printf '%s.%s' "$TTY_TEST_SUCC" "$TTY_RST"
                        NUM_SUCCESSES=$(($NUM_SUCCESSES + 1))
                    else
                        printf '%s*%s' "$TTY_TEST_FAIL" "$TTY_RST"
                        FAILURE=1
                        FAILING_TESTS="$FAILING_TESTS $TEST(gc)"
                    fi
                    NUM_TESTS=$(($NUM_TESTS + 1))
                fi
                ;;
            *)
                ;;
        esac
    else
        printf '%s*%s' "$TTY_TEST_FAIL" "$TTY_RST"
        FAILURE=1
        FAILING_TESTS="$FAILING_TESTS $TEST"
    fi
    cd $WORKING_DIR
    NUM_TESTS=$(($NUM_TESTS + 1))
done
printf '\n'

if [ $FAILURE -eq 0 ]; then
    printf '%sAll %d tests passed %s\n' "$TTY_TEST_SUCC" "$NUM_TESTS" "$TTY_RST"
else
    NUM_FAILED=$(( $NUM_TESTS - $NUM_SUCCESSES ))
    printf '%d out of %d passed, ' "$NUM_SUCCESSES" "$NUM_TESTS"
    printf '%s%d failed%s\n' "$TTY_TEST_FAIL" "$NUM_FAILED" "$TTY_RST"

    printf 'Failing tests: %s\n' "$FAILING_TESTS"
fi

exit $FAILURE
