#!/bin/bash

clang -Iinclude -ggdb test.c -o test && ./test
