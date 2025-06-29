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

3. **Type System** (`src/Language/Memento/TypeSolver/`)

   - Constraint-based type inference and checking
   - Parametric polymorphism with variance analysis
   - Union/intersection types with subtyping
   - Exhaustive pattern matching verification
   - Type schemes for generalization and instantiation

4. **Backend Code Generation**
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

### Coding Style

- **Avoid top-level case splitting**: Prefer using `\case` over top-level case expressions
- **Pattern matching**: Use lambda case expressions for cleaner code

### Architecture Principles

- **Composability**: Design components to be composable and reusable
- **Type safety**: Leverage Haskell's type system for correctness
- **Maintainability**: Write code that is easy to understand and modify
- **Correctness over convenience**: Prioritize correctness in implementation

### Development Guidelines

- Pay close attention to notices and coding guidelines
- Respect the established architectural patterns and design principles
- Carefully implement new features to maintain type safety and consistency
- Follow existing patterns when adding new AST nodes or parser components

## Compiler Usage

### Running the Compiler

```bash
# Parse a Memento source file
stack run -- parse example.mmt

# Type check a Memento source file
stack run -- check example.mmt

# Compile to JavaScript
stack run -- compile example.mmt

# Compile and run
stack run -- run example.mmt

# Or if installed:
memento-compiler parse example.mmt
memento-compiler check example.mmt
memento-compiler compile example.mmt
memento-compiler run example.mmt
```

### Testing Parser and Type Checker

```bash
# Test parsing with the provided example
stack run -- parse example/basic/syntax.mmt

# Test type checking
stack run -- check example/basic/syntax.mmt
```

### Type Checker Implementation Notes

1. **Type Solver**: Located in `src/Language/Memento/TypeSolver.hs`

   - Constraint-based algorithm with iterative solving
   - Supports polymorphism through type schemes
   - Handles union/intersection types with proper subtyping
   - Branch splitting for ambiguous constraints
   - Sound unification ensuring correctness

2. **Type System Features**:

   - **Parametric Polymorphism**: Generic functions with `<T>` parameters
   - **Subtyping**: Structural typing with variance-aware decomposition
   - **Union Types**: `A | B` for sum types
   - **Intersection Types**: `A & B` for intersection types
   - **Pattern Matching**: Exhaustiveness checking via constraint solving
   - **Type Inference**: Automatic type deduction from constraints

3. **Constraint Generation**: Located in `src/Language/Memento/TypeSolver/ConstraintGen.hs`

   - Generates subtype constraints from AST
   - Handles polymorphic instantiation and generalization
   - Pattern matching constraint generation for exhaustiveness
   - Position-aware error reporting

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
   - Add constraint generation logic in `src/Language/Memento/TypeSolver/ConstraintGen.hs`
   - Update type checking if the syntax affects typing rules

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
