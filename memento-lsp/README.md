# Memento Language Server

Language Server Protocol (LSP) implementation for the Memento programming language.

## Features

### Implemented
- [x] Basic server initialization
- [x] Text document synchronization
- [ ] Diagnostics (parse and type errors)
- [ ] Hover information (type information)
- [ ] Go to definition
- [ ] Auto-completion
- [ ] Find references
- [ ] Rename symbol
- [ ] Code formatting

## Installation

### From Source
```bash
# From the monorepo root
stack build memento-lsp
stack install memento-lsp
```

## Editor Configuration

### VS Code
Install the Memento extension (coming soon).

### Neovim
```lua
-- Using nvim-lspconfig
require('lspconfig').memento_lsp.setup{
  cmd = {'memento-lsp-exe'},
  filetypes = {'memento', 'mmt'},
  root_dir = require('lspconfig').util.root_pattern('.git', 'stack.yaml', 'package.yaml'),
}
```

### Other Editors
The LSP server can be used with any editor that supports the Language Server Protocol.

## Development

### Building
```bash
stack build memento-lsp
```

### Running Tests
```bash
stack test memento-lsp
```

### Architecture
The LSP server uses the `lsp` library and integrates with `memento-compiler` for:
- Parsing source files
- Type checking
- Extracting semantic information

## Contributing

Please see the main repository README for contribution guidelines.