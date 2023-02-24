#!/usr/bin/env lua5.3
--
-- Pretty test output
--
-- This is free and unencumbered software released into the public domain.
-- See ../LICENSE.unlicense
--


-- Print without newline, for the test "dots"
function printn(str) 
  io.write(str)
  io.flush()
end


-- TODO: Replace with ncurses.
local get_term_escape_failed = false
function getTermEscape(command)
  if get_term_escape_failed then
    return nil
  end
  local pipe = io.popen("tput "..command)
  if not pipe then
    get_term_escape_failed = true
    return nil
  end
  local result = pipe:read("*a")
  pipe:close()
  return result
end

local term_bold = getTermEscape("bold")
local term_green = getTermEscape("setaf 2")
local term_red = getTermEscape("setaf 1")
local term_yellow = getTermEscape("setaf 3")
local term_cyan = getTermEscape("setaf 6")
local term_reset = getTermEscape("sgr0")
-- If one of these was unsuccesful then we set the terminal escape strings
-- to empty strings and they'll have no effect.
local term_success = ""
local term_failure = ""
local term_skip = ""
local term_todo = ""
if term_bold and term_green and term_red and term_reset then
  term_success = term_green .. term_bold
  term_failure = term_red .. term_bold
  term_skip = term_yellow .. term_bold
  term_todo = term_cyan .. term_bold
end
term_reset = term_reset or ""

local num_tests
local num_ok = 0
local num_fail = 0
local num_skip = 0
local num_todo = 0
local num_todo_ok = 0
local failed_tests = {}
local todo_ok_tests = {}

local line
for line in io.lines() do
  if not num_tests then
    local parse = line:match("^1..(%d+)")
    if parse then
      num_tests = tonumber(parse)
    end
  end

  local parse_skip = line:match("# [sS][kK][iI][pP]")
  local parse_todo = line:match("# [tT][oO][dD][oO]")

  local parse_ok = line:match("^ok")
  if parse_ok then
    if parse_skip then
      num_skip = num_skip + 1
      printn(term_skip .. "-")
    else
      if parse_todo then
        num_todo_ok = num_todo_ok + 1
        printn(term_success .. "?")
        table.insert(todo_ok_tests, line) 
      else
        num_ok = num_ok + 1
        printn(term_success .. ".")
      end
    end
  else
    local parse_nok = line:match("^not ok")
    if parse_nok then
      if parse_skip then
        num_skip = num_skip + 1
        printn(term_skip .. "-")
      elseif parse_todo then
        num_todo = num_todo + 1
        printn(term_todo .. "=")
      else
        num_fail = num_fail + 1
        printn(term_failure .. "+")
        if not status then
          table.insert(failed_tests, line) 
        end
      end
    end
  end
end

-- print a newline
print(term_reset)

if num_ok + num_fail + num_skip + num_todo + num_todo_ok ~= num_tests then
  print("Missing / extra tests:  "..num_ok.." (pass) + "..num_fail.." (fail) + "..num_skip.." (skip) + "..num_todo.." (todo) + "..num_todo_ok.." (todo ok) = "..num_tests)
  os.exit(1)
end

printn(num_ok + num_todo_ok .. " / "..num_tests.." tests passed")

if num_fail ~= 0 or num_skip ~= 0 or num_todo ~= 0 then
  printn(" (")
  if num_fail ~= 0 then
    printn(term_failure..num_fail.." failed"..term_reset)
    if num_skip ~= 0 or num_todo ~= 0 then
      printn(", ")
    end
  end
  if num_todo ~= 0 then
    printn(term_todo..num_todo.." todo"..term_reset)
    if num_skip ~= 0 then
      printn(", ")
    end
  end
  if num_skip ~= 0 then
    printn(term_skip..num_skip.." skipped"..term_reset)
  end
  
  printn(")")
end

print("")

if num_fail ~= 0 then
  print("Failed tests:")
  for _, failed in pairs(failed_tests) do
    print("  "..failed)
  end
end

if num_todo_ok ~= 0 then
  print("Some TODO tests are now passing:")
  for _, todo_ok in pairs(todo_ok_tests) do
    print("  "..todo_ok)
  end
end

if num_fail ~= 0 then
  os.exit(1)
end

