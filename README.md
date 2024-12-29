# compli
My compiler for PLI, the "programming language implementations" course.

This is a work in progress.

## Quickstart

### Syntax of compli
```
// see examples/example.compli

func main(): int = foo(3) + bar(true, 3)

func foo(x: int): int = (
    let double = x + x in (
        if double < 10
        then double
        else x
    )
)

func bar(cond: bool, n: int): int = (
    if cond then n else 0
)
```

### Building a program
```sh
compli input.compli
```
This produces a `myModule.o` with the compiled object code.

The compiler supports a few CLI options to customize its execution.
You can see all the options with the following command:
```sh
compli --help
```

### Runtime & linking
To get an executable you have to compile the [runtime](./rts/rt.c) and link it with the compiled object file.

For example with `gcc`:
```sh
compli input.compli -o compli.o
gcc rts/rt.c compli.o -o my-executable
./my-executable
```

## The compiler pipeline
The rough pipeline steps that a program takes to get from source code to object file are:
1. [Lexing](./src/parsing/lexer.rs) and [parsing](./src/parsing/parser.rs) (source code to [AST](./src/ast.rs))
2. [Type checking](./src/type_checking.rs)
3. [Lowering](./src/lowering.rs) (AST to [IR](./src/ir.rs))
4. [Code generation](./src/codegen.rs) (IR to LLVM IR)

## Dependencies

### LLVM
You need to have **LLVM 18** installed. You can check by running:
```sh
llvm-config --version
```

### Crates
I used some awesome libraries for this compiler. The most interesting ones are:
- [inkwell](https://github.com/TheDan64/inkwell), a LLVM wrapper and building library
- [chumsky](https://github.com/zesterer/chumsky), a text parsing library that uses parser combinators

Also:
- [thiserror](https://github.com/dtolnay/thiserror) and [miette](https://github.com/zkat/miette) for error handling
- [tracing](https://github.com/tokio-rs/tracing) for logging
- [clap](https://github.com/clap-rs/clap) for CLI argument parsing
- [ptree](https://gitlab.com/Noughmad/ptree) for pretty printing of AST and IR
