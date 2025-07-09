#lang racket/base

(require racket/match)
(require racket/list)

(provide compile-to-bytecode
         execute-bytecode
         make-bytecode-vm
         bytecode-instruction?
         bytecode-vm?
         bytecode-vm-stack
         bytecode-vm-push
         bytecode-vm-pop
         optimize-bytecode
         set-bytecode-vm-stack!)

;; Bytecode instruction types
(struct bc-lit (value) #:transparent)                    ; Push literal value
(struct bc-word (name) #:transparent)                    ; Call word/builtin
(struct bc-pop () #:transparent)                         ; Pop from stack
(struct bc-dup () #:transparent)                         ; Duplicate top
(struct bc-swap () #:transparent)                        ; Swap top two
(struct bc-add () #:transparent)                         ; Add top two
(struct bc-sub () #:transparent)                         ; Subtract
(struct bc-mul () #:transparent)                         ; Multiply
(struct bc-div () #:transparent)                         ; Divide
(struct bc-eq () #:transparent)                          ; Equal?
(struct bc-lt () #:transparent)                          ; Less than?
(struct bc-gt () #:transparent)                          ; Greater than?
(struct bc-if (then-code else-code) #:transparent)       ; Conditional
(struct bc-call (quotation) #:transparent)               ; Call quotation
(struct bc-def-word (name code) #:transparent)           ; Define word
(struct bc-builtin (name) #:transparent)                 ; Call builtin function

;; Bytecode VM structure
(struct bytecode-vm (stack words ip code) #:mutable)

(define (make-bytecode-vm)
  (bytecode-vm '() (make-hash) 0 '()))

(define (bytecode-vm-push vm value)
  (set-bytecode-vm-stack! vm (cons value (bytecode-vm-stack vm))))

(define (bytecode-vm-pop vm)
  (match (bytecode-vm-stack vm)
    ['() (error "Stack underflow")]
    [(cons head tail)
     (set-bytecode-vm-stack! vm tail)
     head]))

(define (bytecode-vm-peek vm)
  (match (bytecode-vm-stack vm)
    ['() (error "Empty stack")]
    [(cons head _) head]))

;; Check if something is a bytecode instruction
(define (bytecode-instruction? x)
  (or (bc-lit? x) (bc-word? x) (bc-pop? x) (bc-dup? x) (bc-swap? x)
      (bc-add? x) (bc-sub? x) (bc-mul? x) (bc-div? x) (bc-eq? x)
      (bc-lt? x) (bc-gt? x) (bc-if? x) (bc-call? x) (bc-def-word? x)
      (bc-builtin? x)))

;; Compile tokens to bytecode
(define (compile-to-bytecode tokens)
  (define result '())
  
  (define (compile-token token)
    (cond
      [(number? token) (bc-lit token)]
      [(string? token) (bc-lit token)]
      [(hash? token) (bc-lit token)]
      [(list? token) 
       (cond
         [(and (not (null? token)) (eq? (car token) 'define-word))
          (match token
            [(list 'define-word name body)
             (bc-def-word name (compile-to-bytecode body))])]
         [else (bc-lit token)])]
      [(symbol? token)
       (define name (symbol->string token))
       (cond
         ;; Core stack operations that can be optimized to specific bytecodes
         [(string=? name "drop") (bc-pop)]
         [(string=? name "dup") (bc-dup)]
         [(string=? name "swap") (bc-swap)]
         [(string=? name "+") (bc-add)]
         [(string=? name "-") (bc-sub)]
         [(string=? name "*") (bc-mul)]
         [(string=? name "/") (bc-div)]
         [(string=? name "=") (bc-eq)]
         [(string=? name "<") (bc-lt)]
         [(string=? name ">") (bc-gt)]
         ;; Complex operations that need special handling
         [(string=? name "if")
          ;; This is tricky - need to handle conditional compilation
          ;; For now, treat as word call
          (bc-word name)]
         ;; Everything else is a word call
         [else (bc-word name)])]
      [else (error "Unknown token type in compilation:" token)]))
  
  (for ([token tokens])
    (set! result (cons (compile-token token) result)))
  
  (reverse result))

;; Execute bytecode (pure implementation without VM dependency)
(define (execute-bytecode vm-state bytecode)
  ;; Execute each bytecode instruction directly on bytecode VM
  (for ([instruction bytecode])
    (execute-bytecode-instruction vm-state instruction)))

(define (execute-bytecode-instruction vm instruction)
  (match instruction
    [(bc-lit value)
     (bytecode-vm-push vm value)]
    
    [(bc-pop)
     (bytecode-vm-pop vm)]
    
    [(bc-dup)
     (define val (bytecode-vm-peek vm))
     (bytecode-vm-push vm val)]
    
    [(bc-swap)
     (define a (bytecode-vm-pop vm))
     (define b (bytecode-vm-pop vm))
     (bytecode-vm-push vm a)
     (bytecode-vm-push vm b)]
    
    [(bc-add)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (+ a b))]
    
    [(bc-sub)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (- a b))]
    
    [(bc-mul)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (* a b))]
    
    [(bc-div)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (/ a b))]
    
    [(bc-eq)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (equal? a b))]
    
    [(bc-lt)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (< a b))]
    
    [(bc-gt)
     (define b (bytecode-vm-pop vm))
     (define a (bytecode-vm-pop vm))
     (bytecode-vm-push vm (> a b))]
    
    [(bc-word name)
     ;; For now, push word name as value (simplified implementation)
     (bytecode-vm-push vm (string->symbol name))]
    
    [(bc-def-word name body)
     ;; For now, just add to words hash (simplified)
     (hash-set! (bytecode-vm-words vm) name body)]
    
    [(bc-builtin name)
     ;; For now, push builtin name as value (simplified implementation)
     (bytecode-vm-push vm (string->symbol name))]
    
    [else (error "Unknown bytecode instruction:" instruction)]))

;; Convert bytecode back to tokens (for word definitions)
(define (bytecode-to-tokens bytecode)
  (map (lambda (bc)
         (match bc
           [(bc-lit value) value]
           [(bc-word name) (string->symbol name)]
           [(bc-pop) 'drop]
           [(bc-dup) 'dup]
           [(bc-swap) 'swap]
           [(bc-add) '+]
           [(bc-sub) '-]
           [(bc-mul) '*]
           [(bc-div) '/]
           [(bc-eq) '=]
           [(bc-lt) '<]
           [(bc-gt) '>]
           [else (error "Cannot convert bytecode to token:" bc)]))
       bytecode))

;; Bytecode optimization passes
(define (optimize-bytecode bytecode)
  (define optimized bytecode)
  
  ;; Pass 1: Constant folding
  (set! optimized (constant-fold optimized))
  
  ;; Pass 2: Dead code elimination  
  (set! optimized (eliminate-dead-code optimized))
  
  ;; Pass 3: Peephole optimizations
  (set! optimized (peephole-optimize optimized))
  
  optimized)

(define (constant-fold bytecode)
  ;; Constant folding for arithmetic operations
  (define result '())
  
  (let loop ([remaining bytecode])
    (match remaining
      ['() (reverse result)]
      [(list (bc-lit a) (bc-lit b) (bc-add) rest ...)
       (if (and (number? a) (number? b))
           (loop (cons (bc-lit (+ a b)) rest))
           (begin
             (set! result (cons (bc-lit a) result))
             (loop (cdr remaining))))]
      [(list (bc-lit a) (bc-lit b) (bc-mul) rest ...)
       (if (and (number? a) (number? b))
           (loop (cons (bc-lit (* a b)) rest))
           (begin
             (set! result (cons (bc-lit a) result))
             (loop (cdr remaining))))]
      [(cons first rest)
       (set! result (cons first result))
       (loop rest)])))

(define (eliminate-dead-code bytecode)
  ;; Remove unreachable code (basic implementation)
  bytecode) ; Placeholder

(define (peephole-optimize bytecode)
  ;; Peephole optimizations like dup drop -> nop
  (define result '())
  
  (let loop ([remaining bytecode])
    (match remaining
      ['() (reverse result)]
      [(list (bc-dup) (bc-pop) rest ...)
       ;; dup drop is a no-op
       (loop rest)]
      [(list (bc-lit 0) (bc-add) rest ...)
       ;; Adding 0 is a no-op
       (loop rest)]
      [(list (bc-lit 1) (bc-mul) rest ...)
       ;; Multiplying by 1 is a no-op
       (loop rest)]
      [(cons first rest)
       (set! result (cons first result))
       (loop rest)])))