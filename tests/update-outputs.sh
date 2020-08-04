#!/bin/sh
set -e

# This script will help update test expected outputs for failed tests.  It
# is useful when line numbers within tests or the compiler's error messages
# change.  The user should check the diffs before committing them.

# Only work in tests directories that incorporate compiler error messages.
for TESTDIR in tests/invalid tests/modules-invalid tests/missing; do
    for OUTPUT in $TESTDIR/*.out; do
        # If the glob didn't match anything then output won't exist.
        if [ -e $OUTPUT ]; then
            BASE=$TESTDIR/`basename $OUTPUT .out`
            # Only copy the file if there's already an .exp file.  It's
            # possible there may be a .out file but no .exp if we've
            # switched branches recently.
            if [ -e $BASE.exp ]; then
                mv $OUTPUT $BASE.exp
            fi
        fi
    done
done

