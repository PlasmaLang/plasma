
Plasma Test Suite
=================

Test suite organisation
-----------------------

There are no tests here yet.  The test script will execute the tests in the
`examples` directory.

 * **builtins**: Test builtin functions.
 * **language**: Test language features (syntax and basic semantics).
 * **runtime**: Test runtime features like the GC.
 * **types**: Test the type system.

Running tests
-------------

The test script looks for tests in the directories on its command line.

    $ ./tests/run-tests.lua examples tests

Will execute all the tests in the examples and tests directories.

Adding a new test
-----------------

The test script searches for tests by looking for `*.exp` files within the
`tests/` and `examples/` directories and their sub-directories.  The script
will run the tests as follows

### Ordinary tests

These have an `*.exp` file.  The script will attempt to build or find a
bytecode file with the same basename.  For example for `my_test.exp` it will
try to build/find `my_test.pz`.  It will then run this file and gather the
output.  The test passes if exits with 0 for its exit code AND its output
matches the contents of the `*.exp` file.

Test output may include lines beginning with #, these will be ignored when
comparing with the expected output.

### Test configuration

For each test the test script looks in the .p file to find lines containing
`PLZTEST`.  The next two whitespace seperated tokens on that line are
configuration paramter and its value.  For example:

    // PLZTEST build_type dev

Sets the `build_type` parameter to `dev`.

The recognised parameters are:

**`build_type`**: Either `dev` or `rel`.  In which build should this test be
executed.  If not set then the test runs in all builds.  This is the same
build type as set in `build.mk`.

**`returns`**: The expected exit code for a passing test.  The default is 0.

**`output`**: Which stream contains the output we wish to capture and compare.
`stdout` (the default) or `stderr`.

### Building tests

The script will attempt to build tests by checking for a `BUILD.plz` file in
the same directory.  If it finds one it knows it can build the test by
executing `plzbuild` in that directory.  It passes this step if plzbuild
exits with 0.

Other rules
-----------

 * Don't name your test "plzbuild" that is reserved.

