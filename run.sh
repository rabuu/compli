#!/bin/sh

set -xe

cargo run -- $@ -o out.o
cc rts/rt.c out.o
