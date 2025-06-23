#!/bin/bash

clang -Iinclude -Wno-incompatible-pointer-types -ggdb vector.c -o vector
