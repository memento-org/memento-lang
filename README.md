# Memento Programming Language

This is the official monorepo for the Memento programming language and its associated tools.

## Projects

### [memento-compiler](./memento-compiler/)
The core Memento compiler that compiles Memento source code to JavaScript and WebAssembly.

### [memento-lsp](./memento-lsp/)
Language Server Protocol implementation for Memento, providing IDE support for editors like VS Code, Neovim, and others.

## Getting Started

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- GHC 9.8.4 or later (will be installed by Stack)

### Building
```bash
# Build all projects
stack build

# Build specific project
stack build memento-compiler
stack build memento-lsp
```

### Running
```bash
# Run the compiler
stack exec memento-compiler-exe -- <args>

# Run the LSP server
stack exec memento-lsp-exe
```

## Development

This monorepo uses Stack for managing multiple Haskell packages. Each project has its own `package.yaml` file, and they share common dependencies through the root `stack.yaml`.

### Project Structure
```
memento-lang/
├── stack.yaml           # Shared Stack configuration
├── memento-compiler/    # Core compiler
│   ├── package.yaml
│   ├── src/
│   └── test/
└── memento-lsp/        # Language Server
    ├── package.yaml
    ├── src/
    └── test/
```

## Contributing

Contributions are welcome! Please read the contributing guidelines in each project's README.

## License

This project is licensed under the MIT License - see the [LICENSE](./memento-compiler/LICENSE) file for details.