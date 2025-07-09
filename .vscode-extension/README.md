# Rutile Language Support

This VS Code extension provides syntax highlighting and language support for Rutile, a concatenative programming language.

## Features

- **Syntax Highlighting**: Full syntax highlighting for Rutile code
- **Auto-completion**: Bracket and quote auto-completion
- **Code Folding**: Support for quotations and word definitions
- **Comments**: Line comments with `#` and block comments with `( )`

## Installation

1. Copy this extension to your VS Code extensions folder
2. Reload VS Code
3. Open any `.rtl` file to see syntax highlighting

## Language Features

### Syntax Highlighting

- **Comments**: `#` line comments and `( )` block comments
- **Strings**: Double-quoted strings with escape sequences
- **Numbers**: Integer and floating-point numbers
- **Word Definitions**: `:` and `;` keywords
- **Quotations**: `[` and `]` for code blocks
- **Maps**: `{` and `}` for hash maps
- **Builtin Words**: Stack manipulation, math, I/O operations
- **LLM Words**: AI/ML integration functions
- **Agent Words**: Concurrent programming primitives
- **Control Words**: Conditionals and loops
- **Webhook Sigils**: `@` prefixed webhook listeners

### Auto-completion

- Automatic bracket matching for `[`, `]`, `{`, `}`
- Quote completion for `"`
- Word definition completion for `:` and `;`

## Examples

```rutile
# Hello world program
: hello
  "Hello, World!" print-top
;

# Stack manipulation
1 2 3 dup rot swap

# Quotations and combinators
[1 2 3] [2 *] map

# LLM integration
"What is the weather?" {temperature 0.7} llm-call

# Agent management
[+ 1 2 3] spawn-agent "worker" bind
start-supervisor
"worker" get-local "permanent" "restart-always" supervise-agent

# Webhooks
@webhook.listen [request-handler] "http://localhost:8080/hook"
```

## License

MIT License