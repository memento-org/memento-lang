# Memento Programming Language

This is the official monorepo for the Memento programming language and its associated tools.

## Projects

### [memento-compiler](./memento-compiler/)
The core Memento compiler that compiles Memento source code to JavaScript and WebAssembly.

### [memento-lsp](./memento-lsp/)
Language Server Protocol implementation for Memento, providing IDE support for editors like VS Code, Neovim, and others.

### [vscode-memento-extension](./vscode-memento-extension/)
Visual Studio Code extension for Memento language support, including syntax highlighting, real-time diagnostics, and LSP integration.

## Getting Started

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- GHC 9.8.4 or later (will be installed by Stack)
- [Node.js](https://nodejs.org/) 16.x or later (for VS Code extension development)
- [Visual Studio Code](https://code.visualstudio.com/) (for using the extension)

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

### VS Code Extension

#### Development
```bash
cd vscode-memento-extension
npm install
npm run compile
```

#### Running in Development Mode
1. Open the `vscode-memento-extension` folder in VS Code
2. Press `F5` to launch a new VS Code window with the extension loaded
3. Open any `.mmt` file to test the extension

#### Packaging and Installing
```bash
cd vscode-memento-extension
npm install -g vsce
vsce package
code --install-extension memento-language-support-*.vsix
```

#### Features
- **Syntax Highlighting**: Full syntax highlighting for Memento language constructs
- **Real-time Diagnostics**: Type errors and syntax errors are shown as you type and updated when you save
- **LSP Integration**: Full Language Server Protocol support for intelligent code assistance
- **File Association**: Automatically recognizes `.mmt` files as Memento source files

## Development

This monorepo uses Stack for managing multiple Haskell packages. Each project has its own `package.yaml` file, and they share common dependencies through the root `stack.yaml`.

### Project Structure
```
memento-compiler/
├── stack.yaml               # Shared Stack configuration
├── README.md               # This file
├── memento-compiler/       # Core compiler
│   ├── package.yaml
│   ├── src/
│   └── test/
├── memento-lsp/           # Language Server
│   ├── package.yaml
│   ├── src/
│   └── test/
└── vscode-memento-extension/  # VS Code extension
    ├── package.json
    ├── src/
    │   └── extension.ts   # Extension entry point
    ├── syntaxes/          # Syntax highlighting
    └── test-example.mmt   # Example file for testing
```

## Contributing

Contributions are welcome! Please read the contributing guidelines in each project's README.

## License

This project is licensed under the MIT License - see the [LICENSE](./memento-compiler/LICENSE) file for details.