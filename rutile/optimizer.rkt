#lang racket

(require racket/match)
(require racket/list)

(provide optimize-program
         inline-words
         optimize-tail-calls
         dead-code-elimination
         constant-folding
         peephole-optimize
         optimization-stats)

;; Main optimization entry point
(define (optimize-program tokens word-definitions [level 'standard])
  (define optimized tokens)
  
  ;; Apply optimizations based on level
  (case level
    ['minimal
     (set! optimized (peephole-optimize optimized))]
    
    ['standard  
     (set! optimized (constant-folding optimized))
     (set! optimized (dead-code-elimination optimized))
     (set! optimized (peephole-optimize optimized))
     (set! optimized (inline-words optimized word-definitions 'conservative))]
    
    ['aggressive
     (set! optimized (constant-folding optimized))
     (set! optimized (dead-code-elimination optimized))
     (set! optimized (inline-words optimized word-definitions 'aggressive))
     (set! optimized (optimize-tail-calls optimized))
     (set! optimized (peephole-optimize optimized))])
  
  optimized)

;; Word inlining optimization
(define (inline-words tokens word-definitions inlining-level)
  (define inline-threshold 
    (case inlining-level
      ('conservative 3)  ; Only inline very short words
      ('aggressive 10)   ; Inline longer words
      (else 5)))
  
  ;; For now, just use VM word definitions (skip local extraction due to syntax issues)
  (define all-word-defs word-definitions)
  
  (define inlinable-words 
    (filter (lambda (word-pair)
              (match word-pair
                ((cons name body)
                 (and (< (length body) inline-threshold)
                      (not (contains-recursion? body name))
                      (not (contains-side-effects? body))))))
            all-word-defs))
  
  (inline-tokens tokens inlinable-words))

;; TODO: Extract word definitions from token sequence - disabled due to syntax issues
(define (extract-word-definitions tokens)
  '()) ; Return empty for now

;; TODO: Find the position of semicolon in token list - disabled due to syntax issues
(define (find-semicolon tokens)
  #f) ; Return false for now

;; Simple semicolon finder for tail call optimization  
(define (find-semicolon-simple tokens)
  (if (null? tokens)
      #f
      (if (equal? (car tokens) ";")
          0
          (let ([rest (find-semicolon-simple (cdr tokens))])
            (if rest (+ 1 rest) #f)))))


;; Inline tokens with word substitution
(define (inline-tokens tokens inlinable-words)
  (define (inline-token token)
    (if (symbol? token)
        (let ([word-body (assoc (symbol->string token) inlinable-words)])
          (if word-body
              (cdr word-body)  ; Return the word body for inlining
              (list token)))   ; Return as single-item list
        (list token)))
  
  (flatten (map inline-token tokens)))

;; Check if word body contains recursion
(define (contains-recursion? body word-name)
  (define word-symbol (string->symbol word-name))
  (member word-symbol body))

;; Check if word body contains side effects
(define (contains-side-effects? body)
  (define side-effect-words '(print-top write-file append-file delete-file
                             http-get http-post llm-call webhook-listen
                             hot-reload-enable))
  (any (lambda (token) 
         (and (symbol? token)
              (member token side-effect-words))) 
       body))

;; Tail call optimization
(define (optimize-tail-calls tokens)
  (optimize-tail-calls-in-tokens tokens))

(define (optimize-tail-calls-in-tokens tokens)
  ;; Handle rutile syntax: : word-name ... ;
  (cond
    [(null? tokens) '()]
    [(< (length tokens) 3) tokens]
    [(and (equal? (car tokens) ":") (symbol? (cadr tokens)))
     ;; Found word definition: : name ... ;
     (define word-name (symbol->string (cadr tokens)))
     (define body-and-rest (cddr tokens))
     (define semicolon-pos (find-semicolon-simple body-and-rest))
     (if semicolon-pos
         (let* ((body (take body-and-rest semicolon-pos))
                (rest-tokens (drop body-and-rest (+ semicolon-pos 1))))
           (append (list ":") 
                   (list (cadr tokens))
                   (optimize-tail-calls-in-body body word-name)
                   (list ";")
                   (optimize-tail-calls-in-tokens rest-tokens)))
         ;; No semicolon found, return unchanged
         tokens)]
    [else
     ;; Not a word definition, process recursively
     (cons (car tokens) (optimize-tail-calls-in-tokens (cdr tokens)))]))

(define (optimize-tail-calls-in-body body word-name)
  (cond
    ((null? body) '())
    ((= (length body) 1) body)  ; Single token, no optimization needed
    (else
     (define last-token (last body))
     (define rest-tokens (take body (- (length body) 1)))
     
     ;; Check if last token is a recursive call
     (if (and (symbol? last-token)
              (string=? (symbol->string last-token) word-name))
         ;; Replace recursive call with tail-recursive version
         (append rest-tokens (list (string->symbol (string-append word-name "-tail"))))
         ;; No tail recursion found
         body))))

;; Dead code elimination
(define (dead-code-elimination tokens)
  (remove-unreachable-code tokens))

(define (remove-unreachable-code tokens)
  (define (reachable? token)
    (not (and (list? token) 
              (= (length token) 3)
              (eq? (car token) 'define)
              (never-called? (cadr token) tokens))))
  
  (filter reachable? tokens))

(define (never-called? word-name tokens)
  (define word-symbol (string->symbol word-name))
  (not (any (lambda (token)
              (cond
                [(symbol? token) (eq? token word-symbol)]
                [(list? token) (member word-symbol token)]
                [else #f]))
            (flatten-definitions tokens))))

(define (flatten-definitions tokens)
  (flatten (map (lambda (token)
                  (if (and (list? token) (eq? (car token) 'define))
                      (caddr token)  ; Return word body
                      token))
                tokens)))

;; Constant folding
(define (constant-folding tokens)
  ;; Apply constant folding to the token sequence directly
  (fold-constants-in-sequence tokens))

(define (fold-constants-in-token token)
  (cond
    [(list? token)
     (match token
       [(list 'define name body)
        (list 'define name (fold-constants-in-sequence body))]
       [_ 
        ;; This is a quotation/list - apply constant folding to it as a sequence
        (fold-constants-in-sequence token)])]
    [else token]))

(define (fold-constants-in-sequence tokens)
  (cond
    [(< (length tokens) 3) tokens]
    [else
     (define folded (fold-arithmetic-operations tokens))
     (if (equal? folded tokens)
         tokens  ; No changes, stop
         (fold-constants-in-sequence folded))]))

(define (fold-arithmetic-operations tokens)
  ;; Handle postfix notation: a b + -> (+ a b)
  (fold-postfix-operations tokens '()))

;; Fold constants in postfix notation using a stack simulation
(define (fold-postfix-operations tokens stack)
  (cond
    [(null? tokens)
     ;; Return the remaining stack (should be the result)
     (reverse stack)]
    
    [(number? (car tokens))
     ;; Push number onto stack
     (fold-postfix-operations (cdr tokens) (cons (car tokens) stack))]
    
    [(and (symbol? (car tokens)) 
          (member (car tokens) '(+ - * /))
          (>= (length stack) 2))
     ;; Apply arithmetic operation
     (define op (car tokens))
     (define b (car stack))
     (define a (cadr stack))
     (define rest-stack (cddr stack))
     
     ;; Check if we can fold this operation
     (if (and (number? a) (number? b))
         (let ([result (case op
                        ['+ (+ a b)]
                        ['- (- a b)]
                        ['* (* a b)]
                        ['/ (if (not (= b 0)) (/ a b) 
                               ;; Can't fold division by zero, keep original
                               #f)])])
           (if result
               (fold-postfix-operations (cdr tokens) (cons result rest-stack))
               ;; Division by zero case - keep original
               (append (reverse stack) (cons (car tokens) '()) (cdr tokens))))
         ;; Can't fold non-numbers, reconstruct
         (append (reverse stack) (cons (car tokens) '()) (cdr tokens)))]
    
    [else
     ;; Other token, push to stack  
     (fold-postfix-operations (cdr tokens) (cons (car tokens) stack))]))

;; Peephole optimization
(define (peephole-optimize tokens)
  (apply-peephole-patterns tokens peephole-patterns))

(define peephole-patterns
  (list
   ;; Stack manipulation optimizations
   '((dup drop) -> ())                    ; dup drop = no-op
   '((drop dup) -> (dup))                 ; drop dup = dup
   '((swap swap) -> ())                   ; swap swap = no-op
   '((dup dup) -> (dup dup))             ; Already optimal
   '((over drop) -> (nip))                ; over drop = nip
   
   ;; Arithmetic optimizations  
   '((0 +) -> ())                         ; Adding 0 is no-op
   '((1 *) -> ())                         ; Multiplying by 1 is no-op
   '((0 *) -> (drop 0))                   ; Multiplying by 0
   '((1 /) -> ())                         ; Dividing by 1 is no-op
   
   ;; Logical optimizations
   '((true and) -> ())                    ; true and x = x
   '((false or) -> ())                    ; false or x = x
   '((not not) -> ())                     ; Double negation
   
   ;; Conditional optimizations
   '((true if) -> (drop))                 ; Always take true branch
   '((false if) -> (nip))                 ; Always take false branch
   ))

(define (apply-peephole-patterns tokens patterns)
  (define optimized tokens)
  (for ([pattern patterns])
    (set! optimized (apply-pattern optimized pattern)))
  optimized)

(define (apply-pattern tokens pattern)
  (match pattern
    [(list from '-> to)
     (replace-sequence tokens from to)]
    [_ tokens]))

(define (replace-sequence tokens from to)
  (cond
    [(< (length tokens) (length from)) tokens]
    [(equal? (take tokens (length from)) from)
     (append to (drop tokens (length from)))]
    [(null? tokens) '()]
    [else 
     (cons (car tokens) (replace-sequence (cdr tokens) from to))]))

;; Utility: check if any predicate is true for sequence
(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

;; Optimization statistics
(define (optimization-stats original-tokens optimized-tokens)
  (define original-size (count-instructions original-tokens))
  (define optimized-size (count-instructions optimized-tokens))
  (define reduction (- original-size optimized-size))
  (define percentage (if (> original-size 0)
                        (* 100.0 (/ reduction original-size))
                        0.0))
  
  (hash 'original-size original-size
        'optimized-size optimized-size
        'reduction reduction
        'percentage percentage))

(define (count-instructions tokens)
  (apply + (map (lambda (token)
                  (cond
                    [(list? token) (count-instructions token)]
                    [else 1]))
                tokens)))