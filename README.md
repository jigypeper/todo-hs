# todo-hs

A command-line todo list application written in Haskell.

## Features

- Add, view, update, and remove todo items
- Set priorities (Low, Normal, High) and due dates
- Store todos in YAML format
- Simple command-line interface

## Installation

Ensure you have GHC and Cabal installed, then:

```bash
cabal build
cabal install
```

## Usage

### Initialize
```bash
todo-hs init
```

### Add a todo item
```bash
todo-hs add "Buy groceries"
```

### List all items
```bash
todo-hs list
```

### View specific item
```bash
todo-hs view 1
```

### Update an item
```bash
todo-hs update 1 --title "Buy groceries and cook dinner"
```

### Remove an item
```bash
todo-hs remove 1
```

### Show information
```bash
todo-hs info
```

## Options

- `-p, --data-path`: Specify custom data file path (default: `~/.todo.yaml`)

## Development

### Running tests
```bash
cabal test
```

### Building
```bash
cabal build
```

## License

BSD-3-Clause