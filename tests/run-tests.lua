#!/usr/bin/env lua5.3
--
-- Plasma testing script
--
-- This command expects to be passed directories in which it can find Plasma
-- tests.  Information about how it finds tests can be found in README.md.
-- It uses the TAP protocol (http://testanything.org/tap-specification.html)
-- so that its output may be further processed by other tools including CI.
--
-- This is free and unencumbered software released into the public domain.
-- See ../LICENSE.unlicense
--

-- XXX: I couldn't make strict work, it caught errors, but execution just
-- seemed to stop within dir_recursive()
--
-- local _ENV = require 'std.strict'(_G)

local lfs = require "lfs"


--
-- Constants
-------------

local root_dir = lfs.currentdir()
local plzbuild_bin = root_dir .. "/src/plzbuild"
local plzrun_bin = root_dir .. "/runtime/plzrun"
local build_type = os.getenv("BUILD_TYPE")

--
-- Utility functions
---------------------

function debug(message)
  -- print(message)
end

-- Return an iterator that produces all the files under dirs (an array)
-- recursively.
function dir_recursive(dirs)
  function is_dir(path)
    return path:sub(-1) == "/" or lfs.attributes(path, "mode") == "directory"
  end

  -- A recursive function generates file names
  function list_dir(dir)
    for file in lfs.dir(dir) do
      if (file ~= "." and file ~= "..") then
        local full_name = string.format("%s/%s", dir, file)
        if is_dir(full_name) then
          list_dir(full_name)
        else
          coroutine.yield({dir=dir, file=file})
        end
      end
    end
  end

  return coroutine.wrap(function()
    for _, dir in ipairs(dirs) do
      list_dir(dir)
    end
  end)
end

-- Execute a command and capture the result code.
--
-- Args:
--  dir: the working directory
--  bin: the binary to call
--  args: the arguments
--  mb_input_file: a file name for input, or nil to not redirect input.
--  mb_output_file: a file name for output, or nil to return output as a
--    string.
--  mb_stderr_file: a file name for stderr, or nil to return stderr as a
--    string.
--
-- Returns
--  true/false: Was this command succesful (returnd 0)
--  "exited"/"killed": Did the process terminate itself, or was it killed.
--  Number: The return code / signal number
--  String: The output, if mb_output_file was nil.
--  String: The stderr, if mb_stderr_file was nil.
--
-- A lot of this could be accomplished with popen, however I have a later
-- refinement that I actually want the process control features I can use
-- here.
function execute(dir, bin, args, mb_input_file, mb_output_file, mb_stderr_file)
  local E = require 'posix.errno'
  local U = require 'posix.unistd'
  local W = require 'posix.sys.wait'
  local F = require 'posix.fcntl'
  local S = require 'posix.stdio'
  local P = require 'posix.sys.stat'

  -- Before doing any "work" (creating pipes and opening files) remove any
  -- stale files so that a failure doesn't leave a previous output file in
  -- place.
  if mb_output_file and lfs.attributes(mb_output_file) then
    assert(os.remove(mb_output_file))
  end
  if mb_stderr_file and lfs.attributes(mb_stderr_file) then
    assert(os.remove(mb_stderr_file))
  end

  local mb_output_pipe_read, mb_output_pipe_write
  if (not mb_output_file) then
    mb_output_pipe_read, mb_output_pipe_write = assert(U.pipe())
  end

  local mb_stderr_pipe_read, mb_stderr_pipe_write
  if (not mb_stderr_file) then
    mb_stderr_pipe_read, mb_stderr_pipe_write = assert(U.pipe())
  end

  local child = assert(U.fork())
  if child == 0 then
    -- We are the child.
    lfs.chdir(dir)

    -- Remap our side side of the pipes
    function remap(from, to)
      U.close(to)
      U.dup2(from, to)
      U.close(from)
    end

    -- Remap input first, in case there's an error we can complain with
    -- stdout.
    if (mb_input_file) then
      remap(assert(F.open(mb_input_file, F.O_RDONLY), 0))
    end
    local open_opts = F.O_WRONLY | F.O_TRUNC | F.O_CREAT
    local open_mode = P.S_IRUSR | P.S_IWUSR | P.S_IRGRP | P.S_IWGRP |
        P.S_IROTH | P.S_IWOTH
    if (mb_output_file) then
      remap(assert(F.open(mb_output_file, open_opts, open_mode)), 1)
    else
      U.close(mb_output_pipe_read)
      remap(mb_output_pipe_write, 1)
    end
    if (mb_stderr_file) then
      remap(assert(F.open(mb_stderr_file, open_opts, open_mode)), 2)
    else
      U.close(mb_stderr_pipe_read)
      remap(mb_stderr_pipe_write, 2)
    end

    local _, err = U.execp(bin, args)
    print("Exec of %s failed: %s", bin, err)
    print("Bail out!")
    os.exit(1)
  end

  local output = ""
  if mb_output_pipe_read then
    U.close(mb_output_pipe_write)
    repeat
      local str = U.read(mb_output_pipe_read, 4096)
      if str then
        output = output .. str
      end
    until not str or str == ""
  end

  -- This is a bad way to read two streams since we could deadlock
  -- TODO: make them non-blocking.
  local stderr = ""
  if mb_stderr_pipe_read then
    U.close(mb_stderr_pipe_write)
    repeat
      local str = U.read(mb_stderr_pipe_read, 4096)
      if str then
        stderr = stderr .. str
      end
    until not str or str == ""
  end

  local pid, exit, status
  repeat
    pid, exit, status = W.wait(child, 0)
    if pid == nil then
      debug("wait: " .. exit)
      exit_error('wait')
    end
    debug(string.format("child: %d exit: %s status: %d", pid, exit, status))
  until (pid == child) and (exit == "exited" or exit == "killed")

  -- TODO: if killed by SIGINT we should abort the whole test suite.
  return exit, status, output, stderr
end


--
-- Gather all the tests
------------------------

-- Gather test configuration for this test.
--
-- Parameters:
--  path - the path to the test's .exp file
--
-- Returns:
--  A table containing the keys:
--    expect_return - the test's expected return code
--    check_stderr - if we should check stderr output rather than stdout
--    build_type - The build type to enable this test for (nil for all)
--    test_type - The type of this test (nil for auto or compile_failure)
--
function test_configuration(filename)
  local expect_return = 0
  local check_stderr = false
  local build_type
  local test_type

  function invalid_value(key, value)
    print(string.format("%s: Invalid value '%s' for key %s",
      filename, value, key))
  end

  local file = io.open(filename)
  if file then
    -- File exists, we can parse it for test declarations.
    local line_no = 0
    for line in file:lines() do
      line_no = line_no + 1
      local _, _, key, value = string.find(line, "PLZTEST%s+(%S+)%s+(%S+)")
      if key then
        if key == "returns" then
          expect_return = tonumber(value)
          if not expect_return then
            invalid_value(line_no, key, value)
          end
        elseif key == "output" then
          if value == "stdout" then
            check_stderr = false
          elseif value == "stderr" then
            check_stderr = true
          else
            invalid_value(line_no, key, value)
          end
        elseif key == "build_type" then
          build_type = value 
        elseif key == "type" then
          test_type = value
        else
          print(string.format("%s:%d: Unknown key in test configuration %s",
            filename, line_no, key))
        end
      end
    end
    file:close()
  end

  return {
    expect_return = expect_return,
    check_stderr = check_stderr,
    build_type = build_type,
    test_type = test_type,
  }
end

-- gen_all_tests is a generator of dictionaries. each dictionary has:
--
--  name:    String, the name of the test
--  desc:    String, a description of the test (unique)
--  type:    either "plzbuild" or "run"
--  dir:     String, the working directory for the test
--  output:  String, the file name to write the test output to.
--  depends: nil, anther test that this test needs before it can run.
--
-- plzbuild tests:
--  These tests will run "plzbuild" in a directory, they check that is
--  returns a zero exit code.
--
-- run tests:
--  These tests will run a plasma program, check that it returns 0.
--  and compare its output with an exptected output.
--
--  expect:  String, the path to the expected output file
--  input:   nil or String, the path to an input for stdin
--  program: String, the path to the Plasma bytecode
--
gen_all_tests =
  coroutine.wrap(function()
    local dirs = {}

    for path in dir_recursive(arg) do
      if (path.file:match(".exp$")) then
        if (not dirs[path.dir]) then
          local build_file = string.format("%s/BUILD.plz", path.dir)
          if (lfs.attributes(build_file)) then
            test = {
              name = "BUILD.plz",
              type = "plzbuild",
              dir = path.dir,
              desc = string.format("%s/BUILD.plz", path.dir),
              output = "plzbuild.out",
              config = {},
            }
            dirs[path.dir] = test
            coroutine.yield(test)
          else
            -- We test for this below and need it to be distinct from nil.
            dirs[path.dir] = "none"
          end
        end

        local maybe_input = path.file:gsub(".exp", ".in")
        if not lfs.attributes(
            string.format("%s/%s", path.dir, maybe_input))
        then
          maybe_input = nil
        end

        function name_if_exists(dir, file, new_ext) 
          local path = string.format("%s/%s", dir, file:gsub(".exp", new_ext))
          return lfs.attributes(path) and path or nil
        end

        local build_file = name_if_exists(path.dir, path.file, ".build")
        local source_file = name_if_exists(path.dir, path.file, ".p")

        local name = path.file:gsub(".exp", "")
        local desc = string.format("%s/%s", path.dir, name)
        local dir_build
        if not build_file and dirs[path.dir] ~= "none" then
          dir_build = dirs[path.dir]
        end

        local config

        if build_file then
          config = test_configuration(build_file)
        elseif source_file then
          config = test_configuration(source_file)
        else
          -- Some module tests have neither a build file nor a source file
          config = {}
        end

        coroutine.yield({
          name = name,
          type = config.test_type or "run",
          dir = path.dir,
          desc = desc,
          depends = dir_build,
          build_file = build_file and path.file:gsub(".exp", ".build"),
          output = path.file:gsub(".exp", ".out"),
          expect = path.file,
          input = maybe_input,
          program = path.file:gsub(".exp", ".pz"),
          config = config,
        })
      end
    end
  end)


--
-- These functions format TAP output
-------------------------------------

-- Each of the tap_ functions writes a TAP test output line.  They all take
-- the test and stage as the first two parameters.

test_no = 0

--
-- Write a generic tap output line based on the status (bool) and any extra
-- information.
--
function tap_result(test, stage, status, extra)
  test_no = test_no + 1
  extra = extra and (" # " .. extra) or ""
  local status_str = status and "ok" or "not ok"
  print(string.format("%s %d %s %s%s",
    status_str, test_no, test.desc, stage, extra))
end

--
-- Indicate that this step was skipped.
--
function tap_skip(test, stage, why)
  tap_result(test, stage, true, "SKIP" .. (why and (" " .. why) or ""))
end

--
-- Indicate that this step was executed with the given how it
-- exited ("exited" or "killed") and the exit/signal code.
--
function tap_exec(test, stage, exit, code, expect_return)
  if exit == "exited" then
    if code == expect_return then
      tap_result(test, stage, true)
      return true
    else
      tap_result(test, stage, false,
        string.format("exited with %d expected %d", code, expect_return))
      return false
    end
  else
    tap_result(test, stage, false, "killed by signal " .. code)
    return false
  end
end

-- True if at least one test failed, then we should return non-zero
local a_test_failed = false

--
-- Run a command for testing.
--
function execute_test_command(test, stage, cmd, args, input,
    exp_out, exp_stderr, exp_return)
  exp_return = exp_return or 0

  local exit, status, output, stderr = 
    execute(test.dir, cmd, args, input, exp_out, exp_stderr)
  local r = tap_exec(test, stage, exit, status, exp_return)
  for line in stderr:gmatch("[^\n]+") do
    print("  " .. line)
  end
  if not r then
    a_test_failed = true

    -- Maybe enable this for a verbose mode.
    for line in output:gmatch("[^\n]+") do
      print("  " .. line)
    end
  end
  return r
end

--
-- This function allows chaining of test steps, and will skip a later step
-- when an earlier one fails.  For example:
--
-- local result = true
-- result = test_step(test, "step 1", result, ...)
-- result = test_step(test, "step 2", result, ...)
-- ...
--
function test_step(test, stage, prev_result, func)
  local depends_result = true
  if test.depends then
    depends_result = test.depends.result
  end
  local result
  if test.config.build_type ~= nil and test.config.build_type ~= build_type then
      tap_skip(test, stage,
        string.format("%s test in %s build",
          test.config.build_type, build_type))
      result = false
  elseif not depends_result then
    tap_skip(test, stage, "dependecy failed")
    result = false
  elseif not prev_result then
    tap_skip(test, stage)
    result = false
  else
    result = func()
  end
  test.result = result
  return result
end

--
-- Return the number of steps in this test
--
function test_num_steps(test)
  if test.type == "plzbuild" then
    return 1
  elseif test.type == "compile_failure" then
    return 2
  elseif test.type == "run" then
    if test.build_file then
      return 3
    else
      return 2
    end
  else
    -- Unknown tests are caught later and become part of TAP output.
    return 1
  end
end

local num_tests = 0
local all_tests = {}
for test in gen_all_tests do
  num_tests = num_tests + test_num_steps(test)
  table.insert(all_tests, test)
end

-- The TAP test plan
print("1.." .. num_tests)

--
-- Filter compiler output for processing by diff
--
function filter_compiler_output(dir, input_name, output_name)
  local input = assert(io.open(dir .. "/" .. input_name))
  local output = assert(io.open(dir .. "/" .. output_name, "w"))
  for line in input:lines() do
    if line:match("^[^%s]+plzc ") then
      break
    end
  end
    
  for line in input:lines() do
    if line:match("ninja: build stopped") then
      break
    end
    output:write(line .. "\n") 
  end
  input:close()
  output:close()
end

--
-- Run the test
--
function run_test(test)
  local result = true -- we start in a good state
  if test.type == "plzbuild" then
    result = test_step(test, "build", result,
      function()
        return execute_test_command(test, "build", plzbuild_bin, {}, nil,
          nil, nil, 0)
      end)
  elseif test.type == "run" then
    if (test.build_file) then
      result = test_step(test, "build", result,
        function()
          build_dir = test.name .. ".dir"
          return execute_test_command(test, "build", plzbuild_bin,
            {"--build-file", test.build_file, "--build-dir", build_dir},
            nil, nil, nil, 0)
        end)
    end
    result = test_step(test, "run", result,
      function()
        local exp_stdout = nil
        local exp_stderr = nil
        if (test.config.check_stderr) then
          exp_stderr = test.output
        else
          exp_stdout = test.output
        end
        return execute_test_command(test, "run", plzrun_bin, {test.program},
          test.input, exp_stdout, exp_stderr, test.config.expect_return)
      end)
    result = test_step(test, "diff", result,
      function()
        local filtered_output = test.output:gsub(".out", ".outs")
        -- grep removes entire lines beginning with a #
        -- sed removes the ends of lines after a #
        assert(execute(test.dir, "sh", 
          {"-c", "grep -v '^#' | sed -e 's/#.*$//'"},
          test.output, filtered_output))
        return execute_test_command(test, "diff", "diff",
          {"-u", test.expect, filtered_output}, nil, nil, nil)
      end)
  elseif test.type == "compile_failure" then
    result = test_step(test, "build-failure", result,
      function()
        build_dir = test.name .. ".dir"
        return execute_test_command(test, "build-failure", plzbuild_bin,
          {"--build-file", test.build_file, "--build-dir", build_dir},
          nil, test.output, nil, 1)
      end)
    result = test_step(test, "diff", result,
      function()
        local filtered_output = test.output:gsub(".out", ".outs")
        -- grep removes entire lines beginning with a #
        -- sed removes the ends of lines after a #
        filter_compiler_output(test.dir, test.output, filtered_output)
        return execute_test_command(test, "diff", "diff",
          {"-u", test.expect, filtered_output}, nil, nil, nil)
      end)
  else
    tap_result(test, "unknown test type "..test.type, false)
    return
  end
end

for _, test in pairs(all_tests) do
  run_test(test)
end

os.exit(a_test_failed and 1 or 0)

