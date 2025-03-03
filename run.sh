#!/bin/sh

set -xe

cargo run -- $@ -o out.o
cc runtime.c out.o
