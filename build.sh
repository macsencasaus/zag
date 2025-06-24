#!/bin/bash

clang -Iinclude -Icodegen -Wno-incompatible-pointer-types -ggdb vector.c -o vector
