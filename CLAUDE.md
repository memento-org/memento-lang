# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the project
stack build

# Run tests
stack test

# Run the compiler executable
stack exec memento-compiler-exe

# Interactive REPL
stack ghci

# Build with fast compilation (for development)
stack build --fast

# Clean build
stack clean && stack build
```

## Code Architecture

### Overview

Memento Compiler (mmtc) is a compiler for the Memento programming language that targets JavaScript and WebAssembly. The project uses a sophisticated AST representation with higher-kinded functors.

### Core Architecture

1. **AST Structure** (`src/Language/Memento/Data/AST/`)

   - Uses GADTs and higher-kinded types for type-safe AST representation
   - Each syntax element (BinOp, Expr, Pattern, etc.) follows a consistent pattern with HFunctor instances
   - Tag types (KBinOp, KExpr, etc.) are used for phantom type tagging

2. **Functor Infrastructure** (`src/Language/Memento/Data/Functor/`)

   - `FixedPoint`: Fixed-point combinators for recursive structures
   - `Coproduct`/`Product`: Higher-kinded coproducts and products
   - `Higher`: HFunctor and related abstractions
   - Uses `IsVoidIn` typeclass for impossible cases

3. **Backend Code Generation**
   - JavaScript backend: `src/Language/Memento/Backend/JS/`
   - WebAssembly backend: `src/Language/Memento/Backend/WASM/`
   - Each backend has its own IR and Codegen modules

### Key Design Patterns

1. **Higher-Kinded Functors**: All AST types implement `HFunctor` for generic transformations
2. **GADT Tags**: Phantom types (KBinOp, KExpr, etc.) ensure type safety
3. **Natural Transformations**: Used for AST transformations via `(~>)` type operator

### Development Notes

- The project uses strict GHC warning flags (-Wall, -Wcompat, etc.)
- Language extensions are explicitly declared per module
- Import structure follows: Data.Kind, project modules, then instances
- Ensure all new AST types implement HFunctor and IsVoidIn as appropriate
