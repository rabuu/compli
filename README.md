# compli
My compiler for PLI, the "programming language implementations" course.

## Building/Installation

### LLVM
You need to have **LLVM 18** installed. You can check by running:
```sh
llvm-config --version
```

### Other dependencies
There are some more dependencies that will fail linking if not installed. Namely:
`libffi`, `libz`, `libzstd`, `libxml2`.

## Used crates
I used some awesome libraries for this compiler. The most interesting ones are:
- [inkwell](https://github.com/TheDan64/inkwell), a LLVM wrapper and building library
- [chumsky](https://github.com/zesterer/chumsky), a text parsing library that uses parser combinators

Also:
- [thiserror](https://github.com/dtolnay/thiserror) and [miette](https://github.com/zkat/miette) for error handling
- [tracing](https://github.com/tokio-rs/tracing) for logging
- [clap](https://github.com/clap-rs/clap) for CLI argument parsing
- [ptree](https://gitlab.com/Noughmad/ptree) for pretty printing of AST and IR

## Usage
The compiler CLI supports multiple execution modes:
```sh
compli INPUT-FILE
compli INPUT-FILE --mode ast
compli INPUT-FILE --mode typed-ast
compli INPUT-FILE --mode ir
```

### Example
```sh
compli main.compli
gcc -o main rts/rt.c main.o
```

## Language Tour
In the [language-tour.compli](./examples/language-tour.compli) all language features are shown.

There are also some more [examples](./examples/).

## Editor support
There are syntax and filetype detection files for Vim provided.

Also, there is a very basic Emacs major mode. To use it, open [compli-mode.el](./editor/emacs/compli-mode.el),
`M-x eval-buffer`, open a compli file and then `M-x compli-mode`.
