#!/bin/sh
set -e

# This script will help update test expected outputs for failed tests.  It
# is useful when line numbers within tests or the compiler's error messages
# change.  The user should check the diffs before committing them.

# Only work in tests directories that incorporate compiler error messages.
DIRS="tests-old/invalid 
      tests-old/modules-invalid"

for TESTDIR in $DIRS; do
    for OUTPUT in $TESTDIR/*.out; do
        # If the glob didn't match anything then output won't exist.
        if [ -e $OUTPUT ]; then
            BASE=$TESTDIR/`basename $OUTPUT .out`
            # Only copy the file if there's already an .exp file.  It's
            # possible there may be a .out file but no .exp if we've
            # switched branches recently.  This also has the effect of not
            # updating .expish files, which is good since those must be
            # updated manually.
            if [ -e $BASE.exp ]; then
                mv $OUTPUT $BASE.exp
            fi
        fi
    done
done

# Do the same for the new test suite, this will need tweaking as we
# develop the test suite though.
TESTDIR=tests/types
for OUTPUT in $TESTDIR/*.outs; do
    # If the glob didn't match anything then output won't exist.
    if [ -e $OUTPUT ]; then
        BASE=$TESTDIR/`basename $OUTPUT .outs`
        # Only copy the file if there's already an .exp file.  It's
        # possible there may be a .out file but no .exp if we've
        # switched branches recently.  This also has the effect of not
        # updating .expish files, which is good since those must be
        # updated manually.
        if [ -e $BASE.exp ]; then
            mv $OUTPUT $BASE.exp
        fi
    fi
done


