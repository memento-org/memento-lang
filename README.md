# Memento Compiler (mmtc)

The Memento Compiler is a compiler for the Memento programming language, targeting JavaScript and WebAssembly.

## Features

- **Subtyping** via structual typing.
- **Affine Algebraic Effects** for more composable programs.
- **Reactive Programming** with asynchronous computation.
- **Affine Types** and **Coeffect System** for better memory management.
- **Algebraic Data & Pattern matching** with exhaustiveness checking (planned)
- **Multiple backends**: JavaScript and WebAssembly (planned)

## Current Status

✅ **Functional Compiler** - The compiler now supports parsing, JavaScript code generation, and executable output. (Type checking and inference are in development)

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- [Node.js](https://nodejs.org/) (for running compiled JavaScript)
- GHC 9.8.4 (will be installed automatically by Stack)

### Building from Source

```bash
# Clone the repository
git clone https://github.com/yourusername/memento-compiler.git
cd memento-compiler

# Build the project
stack build

# Install the compiler
stack install
```

## Usage

### Parse a Memento File

```bash
memento-compiler parse example.mmt
```

This will parse the file and display the Abstract Syntax Tree (AST) if successful, or show parse errors if the syntax is invalid.

### Type Check a Memento File (Partial Implementation)

```bash
memento-compiler check example.mmt
```

This will parse the file and perform partial type analysis, displaying a typed AST with unsolved type variables. Full type checking and inference are still in development.

### Compile to JavaScript

```bash
memento-compiler compile example.mmt
```

This will compile the Memento source to JavaScript and save it in the `dist/` directory.

### Compile and Run

```bash
memento-compiler run example.mmt
```

This will compile the Memento source to JavaScript and automatically execute it with Node.js. If your program has a `main` function, it will be called and its result displayed via `console.log()`.

## Example Memento Code

```memento
// Data type definition
data Option<auto> {
  fn Some<T>(value: T) -> Option<T>,
  fn None<T>() -> Option<T>,
};

// Function definition
fn map<T, U>(f: fn (x: T) -> U, opt: Option<T>) -> Option<U> {
  switch (opt) {
    case (Some(value)) -> Some(f(value)),
    case (None()) -> None()
  }
};

// Value definition
val result: Option<number> = map(fn (x: number) -> x + 1, Some(42));

// Main function (entry point)
fn main() -> number {
  42
};
```

## Language Features

### Expressions

- **Literals**: Numbers, strings, booleans
- **Variables**: `x`, `myVar`
- **Function application**: `f(x, y)`
- **Lambda expressions**: `fn (x: Type) -> x + 1`
- **Binary operators**: `+`, `-`, `*`, `/`, `==`, `<`, `>`
- **If expressions**: `if (cond) { then } else { else }`
- **Block expressions**: `{ let x = 1; x + 1 }`
- **Pattern matching**: `switch (expr) { case (pattern) -> result, ... }`

### Definitions

- **Value definitions**: `val x = 42;`
- **Function definitions**: `fn add(x: number, y: number) -> number { x + y };`
- **Data type definitions**: `data List { fn Nil<T>() -> List<T>, fn Cons<T>(T, List<T>) -> List<T> };`
- **Type aliases**: `type NumList = List<number>;`

## Development

### Project Structure

```
src/
├── Language/
│   └── Memento/
│       ├── Parser/          # Parser implementation
│       ├── Data/           # AST and data structures
│       └── Backend/        # Code generators (JS, WASM)
├── app/                    # Main executable
└── test/                   # Test suite
```

### Running Tests

```bash
stack test
```

### Building Documentation

```bash
stack haddock
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

BSD 3-Clause License (see LICENSE file)
