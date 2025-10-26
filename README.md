<div align="center">
  <img src="public/logo.png" alt="Rutile Logo" width="200"/>

  # Rutile

  **Stack-based language for orchestrating Claude Code agents**
</div>

Stack-based language for coordinating multiple Claude agents. Wraps the `claude` CLI with concatenative programming primitives.

## Features

### Core Language
- **Stack-based execution**: All operations work on an implicit data stack
- **Concatenative syntax**: Compose programs by juxtaposing words
- **Word definitions**: Define custom operations with `: word-name ... ;`
- **Rich data types**: Numbers, strings, lists, maps, booleans
- **Bytecode compilation**: Optimized execution with multiple optimization passes

### Claude Integration
- Spawn multiple Claude agents with different system prompts
- `claude-call`, `spawn-claude-agent`, `agent-send` primitives
- Uses `claude --print --output-format json` via subprocess
- Session persistence per agent

### Standard Library
- **Stack operations**: `dup`, `drop`, `swap`, `rot`, `over`, etc.
- **Math & logic**: Arithmetic, comparisons, trigonometry
- **String processing**: Concatenation, splitting, regex support
- **List operations**: `map`, `filter`, `fold`, `each`, list manipulation
- **I/O**: File operations, console input/output
- **HTTP**: Client requests, server routing, webhooks
- **Crypto**: Hashing (SHA256, MD5), Base64, UUID generation
- **Time**: Date/time operations, sleep, timers

### Development Tools
- **Interactive REPL**: `rtl repl` for live development
- **Hot reload**: Live code reloading for rapid iteration
- **Debugger**: Step-through debugging with `rtl debug`
- **Testing framework**: Property-based testing with `rtl test`
- **Static builds**: Generate standalone executables with `rtl build --static`
- **Project scaffolding**: `rtl new project-name`

## Quick Start

### Installation
```bash
# Clone and build (requires Racket)
git clone <repo-url>
cd rutile
raco pkg install
```

### Basic Usage
```bash
# Start the REPL
rtl repl

# Run a script
rtl run script.rtl

# Create a new project
rtl new my-bot

# Run tests
rtl test
```

### Example: Multi-Agent Research
```rut
# Spawn specialized research agents
"You are a technical researcher" "tech" spawn-claude-agent drop
"You are a business analyst" "biz" spawn-claude-agent drop

# Parallel research
"Explain quantum computing" "tech" agent-send
"Market analysis of quantum computing" "biz" agent-send

# Both responses remain on stack for further processing
```

### Example: Code Generation Pipeline
```rut
# Spawn code agents
"You are a code generator" "generator" spawn-claude-agent drop
"You are a code reviewer" "reviewer" spawn-claude-agent drop

# Generate and review code
"Write a Python function for Fibonacci" "generator" agent-send
dup "reviewer" agent-send  # Send generated code for review
```

### Example: Agent-Backed API Service
```rut
# Spawn service agents
"You are a code assistant" "code" spawn-claude-agent drop
"You are a general assistant" "general" spawn-claude-agent drop

# Route requests to appropriate agent
: handle-request ( query agent -- response )
  agent-send
;

# Example usage
"How do I write a Python function?" "code" handle-request
```

## Language Reference

### Stack Operations
```rut
42 dup          # Duplicate top item: [42 42]
"a" "b" swap    # Swap top two: ["b" "a"]  
1 2 3 drop      # Drop top item: [1 2]
1 2 over        # Copy second item to top: [1 2 1]
```

### Control Flow
```rut
: greet ( name -- )
  "Hello, " swap concat "!" concat print
;

42 0 > if "positive" else "negative" then print

5 times "hello" print end
```

### Claude Operations
```rut
# Basic Claude call
"Explain photosynthesis" claude-call

# With system prompt
{system "You are a science teacher"} "Explain photosynthesis" claude-call

# Spawn persistent agent
"You are a helpful assistant" "helper" spawn-claude-agent
"What can you help with?" "helper" agent-send

# Multiple agents working together
"You are a researcher" "researcher" spawn-claude-agent
"You are an analyst" "analyst" spawn-claude-agent

"Research quantum computing" "researcher" agent-send
"Analyze market trends" "analyst" agent-send
```

## Project Structure

```
rutile/
├── rutile/                      # Core language implementation
│   ├── parser.rkt               # Tokenizer and parser
│   ├── vm.rkt                   # Virtual machine
│   ├── bytecode.rkt             # Bytecode compiler
│   ├── stdlib.rkt               # Standard library
│   ├── claude.rkt               # Claude Code integration
│   ├── http.rkt                 # HTTP client
│   └── ...
├── tests/                       # Test suite
├── examples/                    # Example programs
│   ├── multi-agent-research.rtl # Parallel research agents
│   ├── code-pipeline.rtl        # Code generation with review
│   └── agent-api.rtl            # Agent-backed API service
└── main.rkt                     # CLI entry point
```

## Development

The language is implemented in Racket and uses a bytecode VM for execution. Key components:

- **Parser** (`rutile/parser.rkt`): Tokenization and AST generation
- **VM** (`rutile/vm.rkt`): Stack-based execution engine
- **Claude Integration** (`rutile/claude.rkt`): Multi-agent orchestration via Claude Code CLI
- **Optimizer** (`rutile/optimizer.rkt`): Bytecode optimization passes
- **Standard Library** (`rutile/stdlib.rkt`): Built-in words and operations

## Status

Core language works. Claude integration works via subprocess calls. See `examples/` for working multi-agent demos.

## License

Apache-2.0