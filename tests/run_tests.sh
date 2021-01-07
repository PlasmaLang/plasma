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
TEST_GROUP=$1
if [ "$CI" = "true" ]; then
    LONG_OUTPUT=1
else
    LONG_OUTPUT=0
fi

which tput > /dev/null
if [ $? -a "$TERM" != "" ]; then
    if [ 8 -le "$(tput colors)" ]; then
        TTY_TEST_SUCC=$(tput setaf 2)$(tput bold)
        TTY_TEST_FAIL=$(tput setaf 1)$(tput bold)
        TTY_RST=$(tput sgr0)
    fi
fi

for EXPFILE in pzt/*.exp; do
    TESTS="$TESTS ${EXPFILE%.exp}"
done

# plzbuild/ninja won't rebuild things if the compiler binaries change, so
# always rebuild the examples directory.
touch ../examples/*.p

for DIR in valid invalid modules modules-invalid missing ../examples; do
    for EXPFILE in $DIR/*.exp; do
        TESTS="$TESTS ${EXPFILE%.exp}"
    done
done

for TEST in $TESTS; do
    NAME=$(basename $TEST .test)
    DIR=$(dirname $TEST)
    # Wrapping this up in a test and negating it is a bit annoying, but it
    # was the easy way I could redirect the output and errors successfully.

    case "$TEST_GROUP" in
        rel)
            if [ $TEST = valid/allocateLots ]; then
                continue
            fi
            ;;
        gc)
            case "$TEST" in
                valid/die|valid/noentry)
                    continue
                    ;;
                *invalid/*|missing/*|../examples/*)
                    continue
                    ;;
            esac
            ;;
    esac

    cd $DIR
    if [ "$LONG_OUTPUT" = "1" ]; then
        echo -n "$DIR/$NAME..."
    fi

    if [ "$TEST_GROUP" = "gc" ]; then
        TARGET_TYPE=gctest
    else
        TARGET_TYPE=test
    fi

    if make "$NAME.$TARGET_TYPE" >"$NAME.log" 2>&1; then
        if [ "$LONG_OUTPUT" = "1" ]; then
            printf "%s pass%s" "$TTY_TEST_SUCC" "$TTY_RST"
        else
            printf '%s.%s' "$TTY_TEST_SUCC" "$TTY_RST"
        fi
        NUM_SUCCESSES=$(($NUM_SUCCESSES + 1))
    else
        if [ "$LONG_OUTPUT" = "1" ]; then
            printf "%s fail%s" "$TTY_TEST_FAIL" "$TTY_RST"
        else
            printf '%s*%s' "$TTY_TEST_FAIL" "$TTY_RST"
        fi
        FAILURE=1
        FAILING_TESTS="$FAILING_TESTS $TEST"
    fi
    if [ "$LONG_OUTPUT" = "1" ]; then
        printf '\n'
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

    printf 'Failing tests: \n\t%s\n' "$(echo $FAILING_TESTS | sed -e 's/ /\n\t/g')"
fi

exit $FAILURE
