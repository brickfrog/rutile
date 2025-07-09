<div align="center">
  <img src="public/logo.png" alt="Rutile Logo" width="200"/>
  
  # Rutile
  
  **A concatenative programming language for LLM orchestration**
</div>

> **Experimental DSL** - Rutile is an experimental domain-specific language exploring different ways to interact with LLMs through concatenative programming paradigms.

Rutile is a stack-based (concatenative) programming language built on Racket that makes LLM integration and automation workflows simple and composable. Write AI-powered scripts by chaining words together—no boilerplate, no YAML, just pure function composition.

## Features

### Core Language
- **Stack-based execution**: All operations work on an implicit data stack
- **Concatenative syntax**: Compose programs by juxtaposing words
- **Word definitions**: Define custom operations with `: word-name ... ;`
- **Rich data types**: Numbers, strings, lists, maps, booleans
- **Bytecode compilation**: Optimized execution with multiple optimization passes

### LLM Integration
- **OpenAI API support**: Modern Responses API integration
- **Multiple LLM operations**: `llm-call`, `llm-stream`, `llm-multi-turn`
- **Advanced features**: 
  - Image input with `llm-image`
  - Structured output via JSON schemas
  - Function calling and tools
  - Reasoning models support
- **Model configuration**: Set temperature, max tokens, model selection

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

### Example: LLM Text Processing
```rut
# Set up LLM configuration
{temperature 0.3 max_tokens 300} llm-set-config

# Define a translation and summary word
: translate-and-summarize ( text target-language -- summary )
  "Translate this text to " swap concat ": " concat swap concat
  {temperature 0.2} llm-call
  "Summarize this text in 2-3 sentences: " swap concat
  {temperature 0.3} llm-call
;

# Use it
"The quick brown fox jumps over the lazy dog."
"Spanish" translate-and-summarize print
```

### Example: HTTP API Integration
```rut
# Define an API endpoint handler
: handle-request ( request -- response )
  "prompt" swap hash-ref                    # Extract prompt from request
  {temperature 0.7} llm-call               # Call LLM
  {content type "application/json"} swap   # Wrap in JSON response
  hash-set
;

# Start HTTP server
8080 "/api/chat" handle-request http-serve
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

### LLM Operations
```rut
# Basic LLM call
"Explain photosynthesis" llm-call

# With configuration
"Write a poem" {temperature 0.9 max_tokens 100} llm-call

# Multi-turn conversation
"Hello" llm-multi-turn
"What's 2+2?" llm-multi-turn

# Structured output
"List three colors" {type "array" items {type "string"}} llm-json-schema
```

## Project Structure

```
rutile/
├── rutile/           # Core language implementation
│   ├── parser.rkt    # Tokenizer and parser
│   ├── vm.rkt        # Virtual machine
│   ├── bytecode.rkt  # Bytecode compiler
│   ├── stdlib.rkt    # Standard library
│   ├── llm.rkt       # LLM integration
│   ├── http.rkt      # HTTP client
│   └── ...
├── tests/            # Test suite
├── examples/         # Example programs
└── main.rkt          # CLI entry point
```

## Development

The language is implemented in Racket and uses a bytecode VM for execution. Key components:

- **Parser** (`rutile/parser.rkt`): Tokenization and AST generation
- **VM** (`rutile/vm.rkt`): Stack-based execution engine
- **Optimizer** (`rutile/optimizer.rkt`): Bytecode optimization passes
- **Standard Library** (`rutile/stdlib.rkt`): Built-in words and operations

## Status

Rutile is in active development with core language features, LLM integration, HTTP functionality, and development tools complete.

## License

Apache-2.0