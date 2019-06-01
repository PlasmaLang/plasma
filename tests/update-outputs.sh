#!/bin/sh
set -e

# This script will help update test expected outputs for failed tests.  It
# is useful when line numbers within tests or the compiler's error messages
# change.  The user should check the diffs before committing them.

# Only work in tests directories that incorporate compiler error messages.
for TESTDIR in tests/invalid tests/missing; do
    for OUTPUT in $TESTDIR/*.out; do
        mv $OUTPUT $TESTDIR/`basename $OUTPUT .out`.exp
    done
done

