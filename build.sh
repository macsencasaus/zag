#!/bin/bash

clang -Iinclude -Icodegen -Ias -Wno-incompatible-pointer-types -ggdb vector.c -o vector
