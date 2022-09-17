
Plasma Test Suite
=================

Test suite organisation
-----------------------

Tests are organised into the following directories:

 * **build**: Test the build system.
 * **builtins**: Test builtin functions.
 * **language**: Test language features (syntax and basic semantics).
 * **modules**: Test the module system.
 * **runtime**: Test runtime features like the GC.
 * **types**: Test the type system.

Plus the **examples** directory from the root of the project.

Running tests
-------------

The test script looks for tests in the directories on its command line.

    $ ./tests/run-tests.lua examples tests

Will execute all the tests in the examples and tests directories.

Adding a new test
-----------------

The test script searches for tests by looking for `*.exp` files within the
`tests/` and `examples/` directories and their sub-directories.

All tests have an `*.exp` file, this is the "expected output" of the test,
what exactly "output" means depends on the type of the test.

### Ordinary tests

The test script will attempt to build and run these tests by either finding
a `.build` file with the same name as the test (eg if `my_test.exp` exists
then the script will look for `my_test.build`), or use a `BUILD.plz` file in
the same directory which may be shared with multiple tests (see the
`examples/` directory.)

Once build the test script will expect to run a bytecode file with the same
name, eg `my_test.pz`.  It will run the test, optionally with input from a
`my_test.in` file, and check the output against the expected output.

For example for `my_test.exp` it will
try to build/find `my_test.pz`.  It will then run this file and gather the
output.  The test passes if exits with 0 for its exit code AND its output
matches the contents of the `*.exp` file.

Test output may include lines beginning with #, these will be ignored when
comparing with the expected output.

### compile\_failure tests

These tests will attempt to compile a program (using a matching `.build`
file) and compare the compiler's output with the expected output.  A test
can be made a compiler\_failure test using a test configuration line to
specify the type as below.

## Test configuration

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

**'type`**: If specified it must be set to compile\_failure which
indicates that the build step must fail and return the exit code 1 and the
output of the compiler will be checked against the expected output file.
If the test type is unspecified it defaults to 'run' (aka ordinary tests).

### Building tests

The script will attempt to build tests by checking for a `BUILD.plz` file in
the same directory.  If it finds one it knows it can build the test by
executing `plzbuild` in that directory.  It passes this step if plzbuild
exits with 0.

Other rules
-----------

 * Don't name your test "plzbuild" that is reserved.

