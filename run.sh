#!/bin/sh

set -xe

cargo run -- compile $@ -o out.o
cc runtime.c out.o
./a.out
