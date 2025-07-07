#!/bin/bash

clang \
    -Iinclude -Icodegen -Ias \
    -Werror -Wpedantic -Wall -Wextra \
    -Wno-gnu-folding-constant \
    -Wno-error=unused-function \
    -std=c11 \
    -ggdb \
    vector.c \
    -o vector
