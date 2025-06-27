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

🚧 **Early Development** - Currently only parsing is implemented. Type checking and code generation are not yet available.

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- GHC 9.6.7 (will be installed automatically by Stack)

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

### Other Commands (Not Yet Implemented)

```bash
# Type check a file (not implemented)
memento-compiler check example.mmt

# Compile to JavaScript (not implemented)
memento-compiler compile example.mmt
```

## Example Memento Code

```memento
// Data type definition
data Option {
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
val result = map(fn (x) -> x + 1, Some(42));
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
