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


class Memory_Test_State(Enum):
    Passed = 1
    Leak = 2
    Skipped = 3


default_debug_zag_build = "../build/debug/zag"
default_release_zag_build = "../build/release/zag"

parser = argparse.ArgumentParser(description="Regression tester")
parser.add_argument(
    "tests",
    nargs="+",
    help="Path to test",
)
parser.add_argument(
    "--zag-binary",
    dest="zag_binary",
    type=str,
    help=f"path to zag binary, defaults to '{default_debug_zag_build}' then '{default_release_zag_build}",
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
parser.add_argument(
    "--skip-valgrind",
    dest="skip_valgrind",
    action="store_true",
    help="skip valgrind checks",
)

args = parser.parse_args()

tests = [
    "lex",
    "ir",
    "x86_64-linux",
]

test_cmds = {
    "lex": "%zag --lex %s",
    "ir": "%zag --emit-zag-ir %s",
    "x86_64-linux": "%zag -o /tmp/zag_tmp %s && /tmp/zag_tmp",
}

if args.monochrome:
    state_symbols = {
        Test_State.Passed: "p",
        Test_State.IncorrectAnswer: "w",
        Test_State.Crashed: "f",
        Test_State.Skipped: "_",
    }
    memory_symbols = {
        Memory_Test_State.Passed: "m",
        Memory_Test_State.Leak: "l",
        Memory_Test_State.Skipped: "_",
    }
else:
    RESET = "\033[0m"
    state_symbols = {
        Test_State.Passed: f"\033[32mx{RESET}",
        Test_State.IncorrectAnswer: f"\033[33mx{RESET}",
        Test_State.Crashed: f"\033[31mx{RESET}",
        Test_State.Skipped: f"\033[90mx{RESET}",
    }
    memory_symbols = {
        Memory_Test_State.Passed: f"\033[34mx{RESET}",
        Memory_Test_State.Leak: f"\033[35mx{RESET}",
        Memory_Test_State.Skipped: f"\033[90mx{RESET}",
    }

if args.zag_binary is not None:
    if os.path.exists(args.zag_binary):
        zag_binary = args.zag_binary
    else:
        print(f"\nERROR: zag binary not found at {args.zag_binary}", file=sys.stderr)
        exit(1)
else:
    print("Looking for zag binary... ", end="")
    if os.path.exists(default_debug_zag_build):
        zag_binary = default_debug_zag_build
        print(f"found {default_debug_zag_build}")
    elif os.path.exists(default_release_zag_build):
        zag_binary = default_release_zag_build
        print(f"found {default_release_zag_build}")
    else:
        print("\nERROR: zag binary not found", file=sys.stderr)
        exit(1)

if args.skip_valgrind:
    valgrind_path = None
else:
    valgrind_path = args.valgrind_path
    if valgrind_path is None:
        valgrind_path = shutil.which("valgrind")
        if valgrind_path is None:
            print("valgrind not found, skipping valgrind tests", file=sys.stderr)


test_files = []

while args.tests:
    test = args.tests.pop()
    if os.path.isfile(test) and test.endswith(".zag"):
        test_files.append(test)
    elif os.path.isdir(test):
        args.tests.extend([os.path.join(test, p) for p in os.listdir(test)])

COMMENT_PREFIX = "//"
TEST_CMD = "TEST:"

passed = 0
incorrect = 0
crashed = 0
skipped = 0

width = len(max([splitext(basename(x))[0] for x in test_files], key=len))

if not args.update:
    print()
    legend = f"""
      {state_symbols[Test_State.Passed]} - Passed
      {state_symbols[Test_State.IncorrectAnswer]} - Incorrect answer
      {state_symbols[Test_State.Crashed]} - Crashed
      {state_symbols[Test_State.Skipped]} - Not applicable (skipped)

      {memory_symbols[Memory_Test_State.Passed]} - No memory leaks detected
      {memory_symbols[Memory_Test_State.Leak]} - Leak / memory error Detected
      {memory_symbols[Memory_Test_State.Skipped]} - Skipped
    """
    print(f"Legend:{legend}")

    box_horizontal = "═"
    box_vertical = "║"
    box_right = "╔"

    for i, test in enumerate(tests):
        print(" " * (width + 4), end="")
        print(f"{box_vertical}  " * i, end="")
        print(box_right, end="")
        print(box_horizontal * (len(tests) - i - 1) * 3, end="")
        print(f"{box_horizontal} {test}")

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

    if not args.update:
        print(f"  {case_name:>{width}}: ", end="")

    for test in tests:
        if test not in requested_tests:
            if not args.update:
                print(f"{state_symbols[Test_State.Skipped]}{state_symbols[Test_State.Skipped]} ", end="")
            skipped += 1
            continue

        cmd = test_cmds[test]

        cmd = cmd.replace("%zag", os.path.relpath(zag_binary))
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
            print(f"{state_symbols[Test_State.Crashed]}{state_symbols[Test_State.Skipped]} ", end="")
            crashed += 1
            continue

        actual = proc.stdout

        if not os.path.exists(out):
            print(f"{state_symbols[Test_State.Skipped]}{state_symbols[Test_State.Skipped]} ", end="")
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
            print(f"{state_symbols[Test_State.Passed]}", end="")
            passed += 1
        else:
            print(f"{state_symbols[Test_State.IncorrectAnswer]}", end="")
            incorrect += 1

            if args.verbose:
                print(diff.stdout)

        if valgrind_path is not None:
            valgrind_cmd = f"{valgrind_path} \
            --leak-check=full \
            --show-leak-kinds=all \
            --error-exitcode=1 {cmd}"

            valgrind = subprocess.run(
                valgrind_cmd,
                shell=True,
                input=actual,
                capture_output=True,
                text=True,
            )

            if valgrind.returncode == 0:
                print(f"{memory_symbols[Memory_Test_State.Passed]} ", end="")
            else:
                print(f"{memory_symbols[Memory_Test_State.Leak]} ", end="")
        else:
            print(f"{memory_symbols[Memory_Test_State.Skipped]} ", end="")

    if not args.update:
        print()

if args.update:
    exit()

print()
print("Passed:    ", passed)
print("Incorrect: ", incorrect)
print("Crashed:   ", crashed)
print("Skipped:   ", skipped)
