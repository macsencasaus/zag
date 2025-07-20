#!/bin/python3

import argparse
import os
import platform
import shutil
import subprocess
import sys
from enum import Enum
from os.path import basename, splitext

arch = platform.machine()


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
parser.add_argument(
    "--platform",
    dest="platform",
    choices=["x86_64", "aarch64"],
    help=f"platform to run tests, defaults to {arch}",
)
parser.add_argument("--test", dest="test", type=str, help="specific test to run")

args = parser.parse_args()


class Test_Category:
    def __init__(
        self,
        name,
        zag_flags,
        test_suffix,
        platform=None,
        qbe_dependent=False,
        additional_cmds=None,
    ):
        self.name = name
        self.zag_flags = zag_flags

        # suffix to case name for expected file
        # if several tests share the same test_suffix,
        # they compare against the same file
        self.test_suffix = test_suffix

        self.platform = platform
        self.qbe_dependent = qbe_dependent

        self.additional_cmds=additional_cmds

    def __str__(self):
        return self.name


tests = [
    Test_Category(name="lex", zag_flags="--lex", test_suffix="_lex"),
    Test_Category(name="ir", zag_flags="--emit-zag-ir", test_suffix="_ir"),
    Test_Category(
        name="qbe-il",
        zag_flags="--emit-qbe-il",
        test_suffix="_qbe-il",
        qbe_dependent=True,
    ),
    Test_Category(
        name="x86_64-linux",
        zag_flags="-t x86_64-linux -o /tmp/zag_tmp",
        test_suffix="",
        platform="x86_64",
        additional_cmds="/tmp/zag_tmp",
    ),
    Test_Category(
        name="qbe-amd64_sysv",
        zag_flags="-t qbe-amd64_sysv -o /tmp/zag_tmp",
        test_suffix="",
        platform="x86_64",
        additional_cmds="/tmp/zag_tmp",
    ),
    Test_Category(
        name="qbe-arm64",
        zag_flags="-t qbe-arm64 -o /tmp/zag_tmp",
        test_suffix="",
        platform="aarch64",
        additional_cmds="/tmp/zag_tmp",
    ),
]

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
    print("Looking for zag binary... ", end="", flush=True)
    if os.path.exists(default_debug_zag_build):
        zag_binary = default_debug_zag_build
        print(f"found {default_debug_zag_build}")
    elif os.path.exists(default_release_zag_build):
        zag_binary = default_release_zag_build
        print(f"found {default_release_zag_build}")
    else:
        print("\nERROR: zag binary not found", file=sys.stderr)
        exit(1)


zag_help = subprocess.run(
    f"{zag_binary} -h",
    shell=True,
    capture_output=True,
    text=True,
)

qbe_build = False
if "--emit-qbe-il" in zag_help.stdout:
    qbe_build = True

valgrind_path = None
if not args.update and not args.skip_valgrind:
    valgrind_path = args.valgrind_path
    if valgrind_path is None:
        valgrind_path = shutil.which("valgrind")
        if valgrind_path is None:
            print("valgrind not found, skipping valgrind tests", file=sys.stderr)

if args.platform is not None:
    arch = args.platform

test_files = []

while args.tests:
    test = args.tests.pop()
    if os.path.isfile(test) and test.endswith(".zag"):
        test_files.append(test)
    elif os.path.isdir(test):
        args.tests.extend([os.path.join(test, p) for p in os.listdir(test)])

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


def skip():
    if not args.update:
        print(
            f"{state_symbols[Test_State.Skipped]}{state_symbols[Test_State.Skipped]} ",
            end="",
        )


diffs = []


for test_file in test_files:
    case_name, _ = splitext(basename(test_file))

    if not args.update:
        print(f"  {case_name:>{width}}: ", end="")

    for test in tests:
        if (
            (test.platform is not None and arch != test.platform)
            or (test.qbe_dependent and not qbe_build)
            or (args.test is not None and args.test != test.name)
        ):
            skip()
            skipped += 1
            continue

        expected = os.path.join(
            args.expected_path, f"{case_name}{test.test_suffix}.out"
        )

        cmd = f"{zag_binary} {test.zag_flags} {os.path.relpath(test_file)}"

        if test.additional_cmds is not None:
            cmd += f" && {test.additional_cmds}"

        if args.update:
            with open(expected, "w") as out_file:
                subprocess.run(
                    cmd,
                    shell=True,
                    stdout=out_file,
                    stderr=out_file,
                    text=True,
                )
            continue

        if not os.path.exists(expected):
            skip()
            skipped += 1
            continue

        proc = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
        )

        if proc.returncode != 0:
            print(
                f"{state_symbols[Test_State.Crashed]}{state_symbols[Test_State.Skipped]} ",
                end="",
            )
            crashed += 1
            continue

        diff = subprocess.run(
            f"diff -u --suppress-common-lines {expected} -",
            shell=True,
            input=proc.stdout,
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
                diffs.append((expected, diff.stdout))

        if valgrind_path is not None:
            valgrind_cmd = f"{valgrind_path} \
            --leak-check=full \
            --show-leak-kinds=all \
            --error-exitcode=1 {cmd}"

            valgrind = subprocess.run(
                valgrind_cmd,
                shell=True,
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

if args.verbose:
    for expected, diff_stdout in diffs:
        print()
        print(expected)
        print(diff_stdout)
