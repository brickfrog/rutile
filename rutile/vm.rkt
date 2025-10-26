#lang racket/base

(require racket/match)
(require racket/list)
(require racket/string)
(require racket/file)
(require racket/port)
(require racket/date)
(require racket/hash)
(require "parser.rkt")
(require "enhanced-parser.rkt")
(require "errors.rkt")
(require "claude.rkt")
(require "concurrency.rkt")  
(require "webhooks.rkt")
(require "events.rkt")
(require "hotreload.rkt")
(require "bytecode.rkt")
(require "optimizer.rkt")
(require "modules.rkt")
(require json)
(require file/sha1)
(require file/md5)
(require net/base64)
(require net/url)

(provide make-vm
         vm-stack
         vm-words
         vm-return-stack
         vm-claude-context
         vm-push
         vm-pop
         vm-peek
         vm-define-word
         vm-lookup-word
         execute-tokens
         execute-tokens-bytecode
         execute-word
         builtin-word?
         execute-builtin
         set-vm-stack!
         vm-error
         vm-set-source-context
         execute-tokens-with-positions)

(struct vm (stack return-stack words locals error-handlers claude-context current-file current-source) #:mutable)

(define (make-vm)
  (vm '() '() (make-hash) (make-hash) '() (make-claude-context) #f #f))

(define (vm-push vm val)
  (set-vm-stack! vm (cons val (vm-stack vm))))

(define (vm-pop vm)
  (match (vm-stack vm)
    ['() (error "Stack underflow")]
    [(cons head tail)
     (set-vm-stack! vm tail)
     head]))

(define (vm-peek vm)
  (match (vm-stack vm)
    ['() (error "Empty stack")]
    [(cons head _) head]))

(define (vm-define-word vm name body)
  (hash-set! (vm-words vm) name body))

(define (vm-lookup-word vm name)
  (hash-ref (vm-words vm) name #f))

(define (execute-tokens vm tokens)
  (for ([token tokens])
    (execute-token vm token)))

;; Bytecode compilation and execution  
(define (execute-tokens-bytecode vm tokens)
  "Compile tokens to bytecode and execute for better performance"
  (define bytecode (compile-to-bytecode tokens))
  (define optimized-bytecode (optimize-bytecode bytecode))
  
  ;; Create temporary bytecode VM state
  (define bc-vm (make-bytecode-vm))
  (set-bytecode-vm-stack! bc-vm (vm-stack vm))
  
  ;; Execute optimized bytecode
  (execute-bytecode bc-vm optimized-bytecode)
  
  ;; Copy results back to main VM
  (set-vm-stack! vm (bytecode-vm-stack bc-vm)))

(define (execute-token vm token)
  (cond
    [(number? token) (vm-push vm token)]
    [(string? token) (vm-push vm token)]
    [(symbol? token) (execute-word vm token)]
    [(hash? token) (vm-push vm token)]
    [(and (list? token) (not (null? token)) (eq? (car token) 'define-word))
     (match token
       [(list 'define-word name body)
        (vm-define-word vm name body)])]
    [(list? token) (vm-push vm token)]
    [else (error "Unknown token type:" token)]))

(define (execute-word vm word)
  (define name (symbol->string word))
  (cond
    [(vm-lookup-word vm name)
     => (lambda (body) (execute-tokens vm body))]
    [(hash-ref (vm-locals vm) name #f)
     => (lambda (value) (vm-push vm value))]
    [(builtin-word? name)
     (execute-builtin vm name)]
    [else (vm-unknown-word-error vm name)]))

(define (builtin-word? name)
  (or (core-builtin? name)
      (claude-builtin? name)
      (concurrency-builtin? name)
      (webhook-builtin? name)
      (event-source-builtin? name)
      (hot-reload-builtin? name)
      (module-system-builtin? name)))

(define (core-builtin? name)
  (member name '("dup" "drop" "swap" "rot" "over" "+" "-" "*" "/" "="
                 "<" ">" "<=" ">=" "!=" 
                 "and" "or" "not"
                 "if" "then" "else"
                 "true" "false"
                 "pick" "roll" "depth" "2dup" "2drop" "2swap"
                 "nip" "tuck" "3dup" "3drop"
                 "concat" "split" "join" "lines" "fmt" "length" "substr" "string?" "number?" "boolean?"
                 "list?" "null?" "empty?" "cons" "car" "cdr"
                 "map" "filter" "fold" "each" "times" "while"
                 "read-file" "write-file" "append-file" "file-exists?" "delete-file"
                 "read-line" "write-line"
                 "sin" "cos" "tan" "asin" "acos" "atan" "atan2"
                 "log" "log10" "exp" "sqrt" "pow" "abs" "floor" "ceiling" "round"
                 "min" "max" "random" "pi" "e"
                 "now" "time" "date" "timestamp" "sleep"
                 "print-top" "clear-stack" "show-words" "show-stack" "."
                 "compile" "bytecode-exec" "optimize" "show-bytecode"
                 "http-get" "http-post" "json-parse" "json-stringify"
                 "assert" "test-passed" "test-failed"
                 "let" "with-locals" "bind" "unbind" "get-local" "set-local"
                 "http-server" "http-handler" "http-listen" "http-request" "http-response"
                 "async-http-get" "async-http-post" "chunked-response" "stream-response"
                 "regex-match" "regex-replace" "regex-split" "regex-find-all" "regex-test"
                 "try" "catch" "throw" "error-message" "error-type" "finally"
                 "json-encode" "json-decode" "json-get" "json-set" "json-keys"
                 "sha256" "md5" "base64-encode" "base64-decode" "uuid" "hash-equal"
                 "optimize" "inline-words" "peephole-optimize" "fold-constants"
                 "dead-code-elimination" "optimization-stats" "optimize-tail-calls")))

(define (execute-builtin vm name)
  (cond
    [(core-builtin? name) (execute-core-builtin vm name)]
    [(claude-builtin? name) (execute-claude-builtin vm name vm-pop vm-push vm-stack vm-claude-context)]
    [(concurrency-builtin? name) (execute-concurrency-builtin vm name vm-pop vm-push)]
    [(webhook-builtin? name) (execute-webhook-builtin vm name vm-pop vm-push)]
    [(event-source-builtin? name) (execute-event-source-builtin vm name vm-pop vm-push execute-tokens)]
    [(hot-reload-builtin? name) (execute-hot-reload-builtin vm name vm-pop vm-push execute-tokens vm-words vm-define-word)]
    [(module-system-builtin? name) (execute-module-builtin vm name vm-pop vm-push vm-define-word vm-words make-vm execute-tokens vm-lookup-word vm-define-word)]
    [else (error "Unknown builtin:" name)]))

;; Core language operations only
(define (execute-core-builtin vm name)
  (match name
    ;; Stack operations
    ["dup" 
     (define val (vm-peek vm))
     (vm-push vm val)]
    ["drop"
     (vm-pop vm)]
    ["swap"
     (define a (vm-pop vm))
     (define b (vm-pop vm))
     (vm-push vm a)
     (vm-push vm b)]
    ["rot"
     (define a (vm-pop vm))
     (define b (vm-pop vm))
     (define c (vm-pop vm))
     (vm-push vm b)
     (vm-push vm a)
     (vm-push vm c)]
    ["over"
     (define a (vm-pop vm))
     (define b (vm-peek vm))
     (vm-push vm a)
     (vm-push vm b)]
     
    ;; Arithmetic
    ["+"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (+ a b))]
    ["-"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (- a b))]
    ["*"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (* a b))]
    ["/"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (/ a b))]
     
    ;; Comparison
    ["="
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (equal? a b))]
    ["<"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (< a b))]
    [">"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (> a b))]
    ["<="
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (<= a b))]
    [">="
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (>= a b))]
    ["!="
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (not (equal? a b)))]
     
    ;; Logic
    ["and"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (and a b))]
    ["or"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (or a b))]
    ["not"
     (define a (vm-pop vm))
     (vm-push vm (not a))]
     
    ;; Constants
    ["true"
     (vm-push vm #t)]
    ["false"
     (vm-push vm #f)]
     
    ;; Control flow
    ["if"
     (define else-branch (vm-pop vm))
     (define then-branch (vm-pop vm))
     (define condition (vm-pop vm))
     (if condition
         (execute-tokens vm then-branch)
         (execute-tokens vm else-branch))]
         
    ;; Stack manipulation
    ["pick"
     (define n (vm-pop vm))
     (define stack (vm-stack vm))
     (when (or (< n 0) (>= n (length stack)))
       (error "pick: index out of bounds"))
     (vm-push vm (list-ref stack n))]
    ["roll"
     (define n (vm-pop vm))
     (define stack (vm-stack vm))
     (when (or (< n 0) (>= n (length stack)))
       (error "roll: index out of bounds"))
     (define val (list-ref stack n))
     (set-vm-stack! vm (append (take stack n) 
                              (drop stack (+ n 1))))
     (vm-push vm val)]
    ["depth"
     (vm-push vm (length (vm-stack vm)))]
    ["2dup"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm a)
     (vm-push vm b)
     (vm-push vm a)
     (vm-push vm b)]
    ["2drop"
     (vm-pop vm)
     (vm-pop vm)]
    ["2swap"
     (define d (vm-pop vm))
     (define c (vm-pop vm))
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm c)
     (vm-push vm d)
     (vm-push vm a)
     (vm-push vm b)]
    ["nip"
     (define a (vm-pop vm))
     (vm-pop vm)
     (vm-push vm a)]
    ["tuck"
     (define a (vm-pop vm))
     (define b (vm-pop vm))
     (vm-push vm a)
     (vm-push vm b)
     (vm-push vm a)]
    ["3dup"
     (define c (vm-pop vm))
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm a)
     (vm-push vm b)
     (vm-push vm c)
     (vm-push vm a)
     (vm-push vm b)
     (vm-push vm c)]
    ["3drop"
     (vm-pop vm)
     (vm-pop vm)
     (vm-pop vm)]
     
    ;; String operations
    ["concat"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (string-append (format "~a" a) (format "~a" b)))]
    ["split"
     (define delimiter (vm-pop vm))
     (define str (vm-pop vm))
     (vm-push vm (string-split str delimiter))]
    ["join"
     (define separator (vm-pop vm))
     (define lst (vm-pop vm))
     (vm-push vm (string-join (map (lambda (x) (format "~a" x)) lst) separator))]
    ["lines"
     (define lst (vm-pop vm))
     (vm-push vm (string-join (map (lambda (x) (format "~a" x)) lst) "\n"))]
    ["fmt"
     ;; Format string with args: "template %s and %s" arg1 arg2 2 fmt
     (define count (vm-pop vm))
     (define args (reverse (for/list ([i count]) (vm-pop vm))))
     (define template (vm-pop vm))
     (define result
       (let loop ([template template] [args args])
         (cond
           [(null? args) template]
           [(string-contains? template "%s")
            (loop (regexp-replace #rx"%s" template (format "~a" (car args)))
                  (cdr args))]
           [else template])))
     (vm-push vm result)]
    ["length"
     (define val (vm-pop vm))
     (cond
       [(string? val) (vm-push vm (string-length val))]
       [(list? val) (vm-push vm (length val))]
       [else (error "length: not a string or list")])]
    ["substr"
     (define len (vm-pop vm))
     (define start (vm-pop vm))
     (define str (vm-pop vm))
     (vm-push vm (substring str start (+ start len)))]
     
    ;; Type predicates
    ["string?"
     (define val (vm-pop vm))
     (vm-push vm (string? val))]
    ["number?"
     (define val (vm-pop vm))
     (vm-push vm (number? val))]
    ["boolean?"
     (define val (vm-pop vm))
     (vm-push vm (boolean? val))]
    ["list?"
     (define val (vm-pop vm))
     (vm-push vm (list? val))]
    ["null?"
     (define val (vm-pop vm))
     (vm-push vm (null? val))]
    ["empty?"
     (define val (vm-pop vm))
     (vm-push vm (or (null? val) (and (string? val) (= (string-length val) 0))))]
     
    ;; List operations
    ["cons"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (cons a b))]
    ["car"
     (define lst (vm-pop vm))
     (vm-push vm (car lst))]
    ["cdr"
     (define lst (vm-pop vm))
     (vm-push vm (cdr lst))]
     
    ;; Combinators
    ["map"
     (define quotation (vm-pop vm))
     (define lst (vm-pop vm))
     (define result (map (lambda (item)
                          (vm-push vm item)
                          (execute-tokens vm quotation)
                          (vm-pop vm))
                        lst))
     (vm-push vm result)]
    ["filter"
     (define quotation (vm-pop vm))
     (define lst (vm-pop vm))
     (define result (filter (lambda (item)
                             (vm-push vm item)
                             (execute-tokens vm quotation)
                             (vm-pop vm))
                           lst))
     (vm-push vm result)]
    ["fold"
     (define quotation (vm-pop vm))
     (define init (vm-pop vm))
     (define lst (vm-pop vm))
     (define result (foldl (lambda (item acc)
                            (vm-push vm acc)
                            (vm-push vm item)
                            (execute-tokens vm quotation)
                            (vm-pop vm))
                          init lst))
     (vm-push vm result)]
    ["each"
     (define quotation (vm-pop vm))
     (define lst (vm-pop vm))
     (for-each (lambda (item)
                (vm-push vm item)
                (execute-tokens vm quotation))
              lst)]
              
    ;; Loops
    ["times"
     (define quotation (vm-pop vm))
     (define n (vm-pop vm))
     (for ([i (in-range n)])
       (execute-tokens vm quotation))]
    ["while"
     (define body (vm-pop vm))
     (define condition (vm-pop vm))
     (let loop ()
       (execute-tokens vm condition)
       (when (vm-pop vm)
         (execute-tokens vm body)
         (loop)))]
         
    ;; I/O operations
    ["read-file"
     (define filename (vm-pop vm))
     (vm-push vm (file->string filename))]
    ["write-file"
     (define content (vm-pop vm))
     (define filename (vm-pop vm))
     (with-output-to-file filename
       (lambda () (display content))
       #:exists 'replace)]
    ["append-file"
     (define content (vm-pop vm))
     (define filename (vm-pop vm))
     (with-output-to-file filename
       (lambda () (display content))
       #:exists 'append)]
    ["file-exists?"
     (define filename (vm-pop vm))
     (vm-push vm (file-exists? filename))]
    ["delete-file"
     (define filename (vm-pop vm))
     (delete-file filename)]
    ["read-line"
     (vm-push vm (read-line))]
    ["write-line"
     (define line (vm-pop vm))
     (displayln line)]
     
    ;; Math operations
    ["sin"
     (define x (vm-pop vm))
     (vm-push vm (sin x))]
    ["cos"
     (define x (vm-pop vm))
     (vm-push vm (cos x))]
    ["tan"
     (define x (vm-pop vm))
     (vm-push vm (tan x))]
    ["asin"
     (define x (vm-pop vm))
     (vm-push vm (asin x))]
    ["acos"
     (define x (vm-pop vm))
     (vm-push vm (acos x))]
    ["atan"
     (define x (vm-pop vm))
     (vm-push vm (atan x))]
    ["atan2"
     (define y (vm-pop vm))
     (define x (vm-pop vm))
     (vm-push vm (atan y x))]
    ["log"
     (define x (vm-pop vm))
     (vm-push vm (log x))]
    ["log10"
     (define x (vm-pop vm))
     (vm-push vm (log x 10))]
    ["exp"
     (define x (vm-pop vm))
     (vm-push vm (exp x))]
    ["sqrt"
     (define x (vm-pop vm))
     (vm-push vm (sqrt x))]
    ["pow"
     (define y (vm-pop vm))
     (define x (vm-pop vm))
     (vm-push vm (expt x y))]
    ["abs"
     (define x (vm-pop vm))
     (vm-push vm (abs x))]
    ["floor"
     (define x (vm-pop vm))
     (vm-push vm (floor x))]
    ["ceiling"
     (define x (vm-pop vm))
     (vm-push vm (ceiling x))]
    ["round"
     (define x (vm-pop vm))
     (vm-push vm (round x))]
    ["min"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (min a b))]
    ["max"
     (define b (vm-pop vm))
     (define a (vm-pop vm))
     (vm-push vm (max a b))]
    ["random"
     (vm-push vm (random))]
    ["pi"
     (vm-push vm (* 4 (atan 1)))]
    ["e"
     (vm-push vm (exp 1))]
     
    ;; Date/time operations
    ["now"
     (vm-push vm (current-seconds))]
    ["time"
     (vm-push vm (current-milliseconds))]
    ["date"
     (vm-push vm (date->string (current-date)))]
    ["timestamp"
     (vm-push vm (current-seconds))]
    ["sleep"
     (define seconds (vm-pop vm))
     (sleep seconds)]
     
    ;; Debug operations
    ["print-top"
     (displayln (vm-peek vm))
     (flush-output)]
    ["clear-stack"
     (set-vm-stack! vm '())]
    ["show-words"
     (displayln (hash-keys (vm-words vm)))]
    ["show-stack"
     (displayln (vm-stack vm))]
     
    ;; Utility operations
    ["."
     (displayln (vm-pop vm))
     (flush-output)]
     
    ;; Bytecode operations
    ["compile"
     ;; Compile tokens to bytecode: [tokens] compile -> [bytecode]
     (define tokens (vm-pop vm))
     (define bytecode (compile-to-bytecode tokens))
     (vm-push vm bytecode)]
     
    ["bytecode-exec"
     ;; Execute bytecode: [bytecode] bytecode-exec
     (define bytecode (vm-pop vm))
     (define bc-vm (make-bytecode-vm))
     (set-bytecode-vm-stack! bc-vm (vm-stack vm))
     (execute-bytecode bc-vm bytecode)
     (set-vm-stack! vm (bytecode-vm-stack bc-vm))]
     
    ["optimize-bytecode"
     ;; Optimize bytecode: [bytecode] optimize-bytecode -> [optimized-bytecode]
     (define bytecode (vm-pop vm))
     (define optimized (optimize-bytecode bytecode))
     (vm-push vm optimized)]
     
    ["show-bytecode"
     ;; Show bytecode instructions: [bytecode] show-bytecode
     (define bytecode (vm-pop vm))
     (displayln "Bytecode instructions:")
     (for ([i (in-naturals)]
           [instruction bytecode])
       (displayln (format "  ~a: ~a" i instruction)))]
    
    ;; Optimization operations
    ["optimize"
     ;; Optimize code: [tokens] "level" optimize -> [optimized-tokens]
     (define level-str (vm-pop vm))
     (define tokens (vm-pop vm))
     (define level (string->symbol level-str))
     (define word-defs (get-word-definitions vm))
     (define optimized (optimize-program tokens word-defs level))
     (vm-push vm optimized)]
     
    ["inline-words"
     ;; Inline words: [tokens] "level" inline-words -> [inlined-tokens]
     (define level-str (vm-pop vm))
     (define tokens (vm-pop vm))
     (define level (string->symbol level-str))
     (define word-defs (get-word-definitions vm))
     (define inlined (inline-words tokens word-defs level))
     (vm-push vm inlined)]
     
    ["peephole-optimize"
     ;; Peephole optimize: [tokens] peephole-optimize -> [optimized-tokens]
     (define tokens (vm-pop vm))
     (define optimized (peephole-optimize tokens))
     (vm-push vm optimized)]
     
    ["fold-constants"
     ;; Constant folding: [tokens] fold-constants -> [folded-tokens]
     (define tokens (vm-pop vm))
     (define folded (constant-folding tokens))
     (vm-push vm folded)]
     
    ["dead-code-elimination"
     ;; Dead code elimination: [tokens] dead-code-elimination -> [cleaned-tokens]
     (define tokens (vm-pop vm))
     (define cleaned (dead-code-elimination tokens))
     (vm-push vm cleaned)]
     
    ["optimization-stats"
     ;; Get optimization statistics: [original] [optimized] optimization-stats -> {stats}
     (define optimized (vm-pop vm))
     (define original (vm-pop vm))
     (define stats (optimization-stats original optimized))
     (vm-push vm stats)]
     
    ["optimize-tail-calls"
     ;; Tail call optimization: [tokens] optimize-tail-calls -> [optimized-tokens]
     (define tokens (vm-pop vm))
     (define optimized (optimize-tail-calls tokens))
     (vm-push vm optimized)]
    
    [else (error "Core builtin not yet implemented:" name)]))

;; Enhanced error handling functions

;; Set source context for error reporting
(define (vm-set-source-context vm filename source-text)
  (set-vm-current-file! vm filename)
  (set-vm-current-source! vm source-text))

;; Enhanced error reporting
(define (vm-error vm message error-type [suggestions '()] [position #f])
  (define pos (or position (make-source-position 1 1 (vm-current-file vm))))
  (define user-words 
    (map (lambda (k) (if (symbol? k) (symbol->string k) k)) 
         (hash-keys (vm-words vm))))
  (define core-words '("dup" "drop" "swap" "rot" "over" "+" "-" "*" "/" "="
                       "<" ">" "<=" ">=" "!=" "and" "or" "not" "if" "then" "else"
                       "true" "false" "pick" "roll" "depth" "concat" "split" 
                       "length" "substr" "map" "filter" "fold" "each" "times"
                       "while" "read-file" "write-file" "print" "." "stack"))
  (define all-words (append user-words core-words))
  (define auto-suggestions (suggest-corrections message all-words))
  (define all-suggestions (append suggestions auto-suggestions))
  
  (define error-obj (rutile-error message error-type pos all-suggestions))
  (define source-text (or (vm-current-source vm) ""))
  (define formatted-error (format-error-with-context error-obj source-text))
  
  (error formatted-error))

;; Execute tokens with enhanced position tracking
(define (execute-tokens-with-positions vm tokens filename source-text)
  (vm-set-source-context vm filename source-text)
  (with-handlers ([exn:fail? (lambda (e)
                              (define error-msg (exn-message e))
                              (if (string-contains? error-msg "Stack underflow")
                                  (vm-error vm "Stack underflow" 'runtime)
                                  (error error-msg)))])
    (execute-tokens vm tokens)))

;; Enhanced stack underflow checking
(define (vm-pop-safe vm operation-name)
  (match (vm-stack vm)
    ['() (vm-error vm 
                   (format "Stack underflow in operation '~a'" operation-name)
                   'runtime
                   (list (format "Operation '~a' requires at least 1 value on the stack" operation-name)
                         "Check that you have provided all required arguments"
                         "Use 'depth' to check current stack size"))]
    [(cons head tail)
     (set-vm-stack! vm tail)
     head]))

;; Enhanced unknown word error
(define (vm-unknown-word-error vm word-name)
  (define available-words 
    (map (lambda (k) (if (symbol? k) (symbol->string k) k)) 
         (hash-keys (vm-words vm))))
  (vm-error vm 
            (format "Unknown word: ~a" word-name)
            'runtime
            (suggest-unknown-word (format "Unknown word: ~a" word-name) available-words)))

;; Helper function to get word definitions for optimization
(define (get-word-definitions vm)
  (hash->list (vm-words vm)))