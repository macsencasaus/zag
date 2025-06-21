#!/bin/python3

import argparse
import os
import subprocess
import sys

parser = argparse.ArgumentParser(description="Regression test updater")
parser.add_argument(
    "tests",
    nargs="+",
    help="Path to test to update",
)
parser.add_argument(
    "--vector-binary",
    dest="vector_binary",
    type=str,
    help="path to vector binary",
)

args = parser.parse_args()

if args.vector_binary == None:
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

tests: list[str] = args.tests

test_files = []

while tests:
    test = tests.pop()
    if os.path.isfile(test) and test.endswith(".v3"):
        test_files.append(test)
    elif os.path.isdir(test):
        tests.extend([os.path.join(test, p) for p in os.listdir(test)])

COMMENT_PREFIX = "//"
RUN_CMD = "RUN:"

for test in test_files:
    with open(test, "r") as file:
        cmd = file.readline().strip()

        if not cmd.startswith(COMMENT_PREFIX):
            print(
                f"ERROR: expected RUN comment first line of test {test}",
                file=sys.stderr,
            )
            continue
        cmd = cmd[len(COMMENT_PREFIX) :].strip()

        if not cmd.startswith(RUN_CMD):
            print(
                f"ERROR: expected RUN comment first line of test {test}",
                file=sys.stderr,
            )
            continue
        cmd = cmd[len(RUN_CMD) :].strip()

        cmd = cmd.replace("%vector", os.path.relpath(vector_binary))
        cmd = cmd.replace("%s", os.path.relpath(test))

        out = test[:-3] + ".out"

        with open(out, "w") as out_file:
            subprocess.run(
                cmd,
                shell=True,
                stdout=out_file,
                stderr=out_file,
                text=True,
            )
