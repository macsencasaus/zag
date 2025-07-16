#!/bin/bash

cc \
    -Isrc/thirdparty -Isrc/codegen \
    -Werror -Wpedantic -Wall -Wextra \
    -Wno-implicit-fallthrough \
    -std=c11 \
    -ggdb \
    src/vector.c \
    -o vector
