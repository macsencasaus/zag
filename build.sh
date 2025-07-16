#!/bin/bash

clang \
    -Isrc/thirdparty -Isrc/codegen \
    -Werror -Wpedantic -Wall -Wextra \
    -Wno-gnu-folding-constant \
    -Wno-error=unused-function \
    -std=c11 \
    -ggdb \
    src/vector.c \
    -o vector
