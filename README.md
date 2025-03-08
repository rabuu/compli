# compli
My compiler for PLI, the "programming language implementations" course.

## Overview
`compli` is a functional-style programming language based on the language we implemented in the first six homeworks.

On a high level, the compiler pipeline consists of:
- [Parsing](./src/parsing/mod.rs) (source code to [AST](./src/ast.rs))
- [Type checking](./src/type_checking.rs) (provide type information in AST)
- [Lowering](./src/lowering.rs) (typed AST to [compli's IR](./src/ir.rs))
- [Code generation](./src/codegen.rs) (compli's IR to LLVM IR)

The main differences compared to the language we developed in the course are:
- My custom language design (with major additions, see next paragraph)
- Implementation specifics (used programming language & libraries)
- Much higher-level intermediate representation (thanks to the `inkwell` builder)

### Language & compiler features
The highlights of my compiler project are:
- Capable front-end
    - Inspectable AST and IR (with pretty tree view)
    - Powerful type checker (allows for type-dependent lowering)
    - Nice error reports
- Floating-point numbers
- Records (product types)
- [Runtime](./runtime.c) with useful builtins

### Language Tour & example programs
In the [language-tour.compli](./examples/language-tour.compli) all language features are shown.

There are also some more [examples](./examples/).

## Building/Installation
You need to have installed:
- A recent Rust toolchain
- LLVM 18 (see [llvm-sys documentation](https://gitlab.com/taricorp/llvm-sys.rs#build-requirements) for more information)
- Additional libraries for linking
    - I needed to install: `libz`, `libzstd`, `libffi`

This project is Linux-only.

### Example setup: Debian 12 (Bookworm)
```sh
apt install build-essential libzstd-dev zlib1g-dev libffi-dev

wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key > /etc/apt/trusted.gpg.d/llvm.asc
echo 'deb http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-18 main' > /etc/apt/sources.list.d/llvm-18.list
apt update
apt install llvm-18-dev libpolly-18-dev

cargo build
```
- For other Debian/Ubuntu versions: see <https://apt.llvm.org/>
- For Arch Linux: `pacman -S llvm18`

## Usage
The compiler has multiple execution modes:
- `compli build main.compli` will compile the source code and automatically calls to the system C compiler to link the module with the runtime.
This produces a `main` ELF executable.
- `compli generate main.compli` will only generate the compiled object (or assembly) file but won't link anything.
This produces a `main.o` object file.
- `compli inspect-ast main.compli` will print the program's AST. With the `--typed` flag, it includes type information.
- `compli inspect-ir main.compli` will print the intermediate representation of the program.

See `compli --help` for all options.

### Example usage
```sh
compli build main.compli
./main

# with manual linking
compli generate main.compli
gcc runtime.c main.o -o main
./main
```

## Editor support (syntax highlighting)

#### Vim
There are [syntax](./editor/vim/syntax/compli.vim) and [filetype detection](./editor/vim/ftdetect/compli.vim) files for Vim provided
(which can be copied/symlinked to the `$VIMHOME/syntax/` and `$VIMHOME/ftdetect/` directories).

#### Emacs
Also, there is a very basic Emacs major mode. To use it, open [compli-mode.el](./editor/emacs/compli-mode.el),
`M-x eval-buffer`, open a compli file and then `M-x compli-mode`.

## Used crates
I used some awesome libraries for this compiler. The most interesting ones are:
- [inkwell](https://github.com/TheDan64/inkwell), a LLVM wrapper and building library
- [chumsky](https://github.com/zesterer/chumsky), a text parsing library that uses parser combinators

Also:
- [thiserror](https://github.com/dtolnay/thiserror) and [miette](https://github.com/zkat/miette) for error handling/reporting
- [tracing](https://github.com/tokio-rs/tracing) for logging
- [clap](https://github.com/clap-rs/clap) for CLI argument parsing
- [ptree](https://gitlab.com/Noughmad/ptree) for pretty printing of AST and IR
- [tempfile](https://github.com/Stebalien/tempfile) for temporary files
