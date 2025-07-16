#!/bin/python3

import argparse
import os
import shutil
import subprocess
import sys
from enum import Enum
from os.path import basename, splitext


class Test_State(Enum):
    Passed = 1
    IncorrectAnswer = 2
    Crashed = 3
    Skipped = 4


parser = argparse.ArgumentParser(description="Regression tester")
parser.add_argument(
    "tests",
    nargs="+",
    help="Path to test",
)
parser.add_argument(
    "--vector-binary",
    dest="vector_binary",
    type=str,
    help="path to vector binary, default: '../build/debug/vector'",
)
parser.add_argument(
    "-v", "--verbose", action="store_true", help="diff print out on fail"
)
parser.add_argument(
    "--valgrind-path",
    dest="valgrind_path",
    type=str,
    help="path to valgind binary, defaults to valgrind in PATH",
)
parser.add_argument(
    "--expected-path",
    dest="expected_path",
    type=str,
    help="path to expected directory, default: 'expected'",
    default="expected",
)
parser.add_argument(
    "-M",
    "--monochrome",
    dest="monochrome",
    action="store_true",
    help="print in monochrome",
)
parser.add_argument(
    "-u",
    "--update",
    dest="update",
    action="store_true",
    help="update test expected with test output",
)

args = parser.parse_args()

tests = [
    "lex",
    "ir",
    "x86_64-linux",
]

test_cmds = {
    "lex": "%vector -lex %s",
    "ir": "%vector -emit-ir %s",
    "x86_64-linux": "%vector -stdout %s > /tmp/tmp.o && objdump -M intel -d /tmp/tmp.o",
}

if args.monochrome:
    state_symbols = {
        Test_State.Passed: "p",
        Test_State.IncorrectAnswer: "w",
        Test_State.Crashed: "f",
        Test_State.Skipped: "_",
    }
else:
    RESET = "\033[0m"
    state_symbols = {
        Test_State.Passed: f"\033[32mx{RESET}",
        Test_State.IncorrectAnswer: f"\033[33mx{RESET}",
        Test_State.Crashed: f"\033[31mx{RESET}",
        Test_State.Skipped: f"\033[90mx{RESET}",
    }

if args.vector_binary is None:
    default_vector_binary = "vector"
    one_back = os.path.join("..", default_vector_binary)
    if os.path.exists(default_vector_binary):
        vector_binary = default_vector_binary
    elif os.path.exists(one_back):
        vector_binary = one_back
    else:
        parser.print_usage(file=sys.stderr)
        print("ERROR: vector binary not found", file=sys.stderr)
        exit(1)
else:
    vector_binary = args.test_path

valgrind_path = args.valgrind_path
if not valgrind_path:
    valgrind_path = shutil.which("valgrind")
    if valgrind_path is None:
        print("valgrind not found, skipping valgrind tests", file=sys.stderr)


test_files = []

while args.tests:
    test = args.tests.pop()
    if os.path.isfile(test) and test.endswith(".v3"):
        test_files.append(test)
    elif os.path.isdir(test):
        args.tests.extend([os.path.join(test, p) for p in os.listdir(test)])

COMMENT_PREFIX = "//"
TEST_CMD = "TEST:"

passed = 0
incorrect = 0
crashed = 0
skipped = 0

results = {}

for test_file in test_files:
    with open(test_file, "r") as file:
        cmd = file.readline().strip()

    err_str = f"ERROR: expected TEST command comment first line of test {test_file}"

    if not cmd.startswith(COMMENT_PREFIX):
        print(err_str, file=sys.stderr)
        continue
    cmd = cmd[len(COMMENT_PREFIX) :].strip()

    if not cmd.startswith(TEST_CMD):
        print(err_str, file=sys.stderr)
        continue

    cmd = cmd[len(TEST_CMD) :].strip()
    requested_tests = set(map(lambda c: c.strip(), cmd.split(",")))

    case_name, _ = splitext(basename(test_file))
    case_result = {}

    for test in tests:
        if test not in requested_tests:
            case_result[test] = Test_State.Skipped
            skipped += 1
            continue

        cmd = test_cmds[test]

        cmd = cmd.replace("%vector", os.path.relpath(vector_binary))
        cmd = cmd.replace("%s", os.path.relpath(test_file))

        out = os.path.join(args.expected_path, f"{case_name}_{test}.out")

        if args.update:
            with open(out, "w") as out_file:
                subprocess.run(
                    cmd,
                    shell=True,
                    stdout=out_file,
                    stderr=out_file,
                    text=True,
                )
            continue

        proc = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
        )

        if proc.returncode != 0:
            case_result[test] = Test_State.Crashed
            crashed += 1
            continue

        actual = proc.stdout


        if not os.path.exists(out):
            case_result[test] = Test_State.Skipped
            skipped += 1
            continue

        diff = subprocess.run(
            f"diff -u --suppress-common-lines {out} -",
            shell=True,
            input=actual,
            capture_output=True,
            text=True,
        )

        if diff.returncode == 0:
            case_result[test] = Test_State.Passed
            passed += 1
        else:
            case_result[test] = Test_State.IncorrectAnswer
            incorrect += 1

            if args.verbose:
                print(diff.stdout)

    results[case_name] = case_result

if args.update:
    exit()

print()
legend = f"""
  {state_symbols[Test_State.Passed]} - Passed
  {state_symbols[Test_State.IncorrectAnswer]} - Incorrect Answer
  {state_symbols[Test_State.Crashed]} - Crashed
  {state_symbols[Test_State.Skipped]} - Not Applicable (skipped)
"""
print(f"Legend:{legend}")

box_horizontal = "═"
box_vertical = "║"
box_right = "╔"

width = len(max(results, key=len))

for i, test in enumerate(tests):
    print(" " * (width + 4), end="")
    print(f"{box_vertical} " * i, end="")
    print(box_right, end="")
    print(box_horizontal * (len(tests) - i - 1) * 2, end="")
    print(f" {test}")

for case_name, case_result in results.items():
    print(f"  {case_name:>{width}}: ", end="")
    for test in tests:
        print(f"{state_symbols[case_result[test]]} ", end="")
    print()

print()
print("Passed:    ", passed)
print("Incorrect: ", incorrect)
print("Crashed:   ", crashed)
print("Skipped:   ", skipped)
