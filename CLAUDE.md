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

## Repository Author's Preferences

- Pay close attention to notices and coding guidelines
- Respect the established architectural patterns and design principles
- Carefully implement new features to maintain type safety and consistency

## Compiler Usage

### Running the Compiler

```bash
# Parse a Memento source file
stack run -- parse example.mmt

# Or if installed:
memento-compiler parse example.mmt
```

### Testing Parser

```bash
# Test with the provided example
stack run -- parse example/basic/syntax.mmt
```

### Parser Implementation Notes

1. **Expression Parser**: Located in `src/Language/Memento/Parser/Expr.hs`

   - Supports binary operators with proper precedence using `makeExprParser`
   - Function application with chaining: `f(x)(y)(z)`
   - Lambda expressions: `fn (x: Type) -> body`
   - Pattern matching: `switch (expr) [pattern -> result, ...]`
   - Block expressions with let bindings

2. **Propagate Function**: Used for metadata propagation in binary operators

   - Signature: `(HFix h a -> HFix h a -> h2 (HFix h) a) -> HFix h a -> HFix h a -> HFix h a`
   - Combines metadata from left and right operands

3. **Adding New Syntax**: When adding new AST nodes:
   - Add the constructor to the appropriate data type in `src/Language/Memento/Data/AST/`
   - Add the type to the `Syntax` coproduct in `src/Language/Memento/Data/AST.hs`
   - Implement HFunctor and IsVoidIn instances
   - Create parser in appropriate module under `src/Language/Memento/Parser/`
   - Add to `parseAST` in `src/Language/Memento/Parser.hs`

### Syntax Reference

#### Data Type Definitions
```memento
data Option {
  fn Some<T>(value: T) -> Option<T>,
  fn None<T>() -> Option<T>,
};
```

#### Function Definitions
```memento
// Regular function
fn add(a: number, b: number) -> number {
  a + b
};

// Generic function
fn map<T, U>(f: fn (x: T) -> U, opt: Option<T>) -> Option<U> {
  switch (opt) {
    case (Some(value)) -> Some(f(value)),
    case (None()) -> None()
  }
};
```

#### Value Definitions
```memento
// Simple value
val x: number = 42;

// Function value
val increment: fn (x: number) -> number = fn (x: number) -> {
  x + 1
};
```

#### Lambda Expressions
```memento
fn (x: number) -> x + 1
fn (x: number, y: number) -> { x + y }
```

#### Switch/Pattern Matching
```memento
switch (expr) {
  case (Some(x)) -> x,
  case (None()) -> 0
}
```
