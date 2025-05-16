# C-ringe Compiler

This is a very much work-in-progress C-based compiler project designed as a demonstration of my systems programming, compiler construction, and engineering skills. This repository is intended as a portfolio piece to showcase my ability to design, implement, and document a non-trivial software system from scratch.

## Overview

Cringe is a no-library work-in-progress compiler written in C, featuring a modular architecture and a focus on clarity, maintainability, and extensibility. The project includes a custom shift-based DFA lexer, parser, semantic analysis, optimizing backend and code generator that targets x64 for now.

## Features

- **Shift-Based DFA Lexer**: A custom lexer that uses a shift-based DFA approach for tokenization.
- **Handwritten Parser**: No parser generators; This is a recursive descent parser implemented as a state-machine with a manually allocated stack, so there is no risk of stack-overflow and error handling is simple. Only a very small subset of C is parsed for now.
- **Modular Design**: Clear separation between front-end (lexing, parsing, semantic analysis) and back-end (code generation, optimization).
- **x64 Code Generation**: For now, displays generated assembly code for x64 architecture. Only naive register allocation and spilling has been implemented.

- **Build-time Meta-programs**: The lexer and code-generator use meta-programs and domain specific languages to automate the generation of code at compiler build time. This allows for a more declarative approach to defining the compiler's behavior. A meta program generates and encodes the DFA table used by the tokenizer. A descriptor specifies global instruction selection tiling, instruction format and code emission for the backend.

- **Optimizing backend**: Right now, the backend includes a naive graph-coloring register allocator and a few peephole and global optimizations. This includes load-forwarding, dead-code/control-flow elimination, load-forwarding and dead-store elimination. For now the backend performs a naive global instruction selection before global scheduling, where it effectively provides loop-invariant code motion.

## Project Structure

- `cringe/` — Core compiler source code (front-end, back-end, utilities)
- `meta/`   — Meta-programs for generating compiler tables
- `build/`  — Build artifacts and Visual Studio project files

## Getting Started

### Prerequisites
- C compiler (MSVC, GCC, or Clang)
- [CMake](https://cmake.org/) 3.15+

### Building (Windows)
1. Clone the repository:
   ```sh
   git clone https://github.com/jeremyylimmm/c-ringe.git
   cd cringe
   ```
2. Generate Visual Studio solution:
   ```sh
   cmake -S . -B build -G "Visual Studio 17 2022"
   ```
3. Build using Visual Studio or:
   ```sh
   cmake --build build --config Debug
   ```