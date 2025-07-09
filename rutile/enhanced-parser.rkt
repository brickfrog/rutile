#lang racket

(require racket/string)
(require racket/match)
(require json)
(require racket/list)
(require racket/hash)
(require "errors.rkt")

(provide parse-rutile-with-positions
         tokenize-with-positions
         positioned-token
         positioned-token-value
         positioned-token-position)

;; Token with position information
(struct positioned-token (value position) #:transparent)

;; Parse rutile with position tracking
(define (parse-rutile-with-positions input [filename #f])
  (define tokens (tokenize-with-positions input filename))
  (parse-tokens-with-positions tokens))

;; Enhanced tokenizer that tracks positions
(define (tokenize-with-positions input [filename #f])
  (define clean-input (regexp-replace* #rx"#[^\n]*" input ""))
  (define no-paren-comments (remove-paren-comments clean-input))
  (define tokens (tokenize-with-strings-and-positions (string-trim no-paren-comments) filename))
  (filter (lambda (tok) 
            (and (positioned-token? tok)
                 (not (string=? (positioned-token-value tok) "")))) tokens))

;; Core tokenizer with position tracking
(define (tokenize-with-strings-and-positions input filename)
  (define result '())
  (define current-token "")
  (define in-string? #f)
  (define line 1)
  (define column 1)
  (define token-start-line 1)
  (define token-start-column 1)
  
  (define (save-token)
    (when (not (string=? current-token ""))
      (define pos (make-source-position token-start-line token-start-column filename))
      (set! result (cons (positioned-token current-token pos) result))
      (set! current-token "")))
  
  (define (start-new-token)
    (set! token-start-line line)
    (set! token-start-column column))
  
  (for ([char (string->list input)])
    (cond
      [(char=? char #\")
       (if in-string?
           (begin
             (set! current-token (string-append current-token "\""))
             (save-token)
             (set! in-string? #f))
           (begin
             (save-token)
             (start-new-token)
             (set! current-token "\"")
             (set! in-string? #t)))
       (set! column (+ column 1))]
      
      [in-string?
       (set! current-token (string-append current-token (string char)))
       (if (char=? char #\newline)
           (begin (set! line (+ line 1)) (set! column 1))
           (set! column (+ column 1)))]
      
      [(or (char=? char #\[) (char=? char #\]) (char=? char #\{) (char=? char #\}))
       (save-token)
       (start-new-token)
       (set! current-token (string char))
       (save-token)
       (set! column (+ column 1))]
      
      [(char-whitespace? char)
       (save-token)
       (if (char=? char #\newline)
           (begin (set! line (+ line 1)) (set! column 1))
           (set! column (+ column 1)))]
      
      [else
       (when (string=? current-token "")
         (start-new-token))
       (set! current-token (string-append current-token (string char)))
       (set! column (+ column 1))]))
  
  (save-token)
  (reverse result))

;; Remove parenthetical comments
(define (remove-paren-comments input)
  (regexp-replace* #rx"\\([^)]*\\)" input ""))

;; Parse tokens with position information
(define (parse-tokens-with-positions tokens)
  (let loop ([tokens tokens] [result '()])
    (match tokens
      ['() (reverse result)]
      [(cons (positioned-token ":" pos) rest)
       (let-values ([(def-token remaining) (parse-definition-with-positions rest)])
         (loop remaining (cons def-token result)))]
      [(cons (positioned-token "{" pos) rest)
       (let-values ([(map-token remaining) (parse-map-with-positions rest)])
         (loop remaining (cons map-token result)))]
      [(cons (positioned-token "[" pos) rest)
       (let-values ([(list-token remaining) (parse-list-with-positions rest)])
         (loop remaining (cons list-token result)))]
      [(cons token rest)
       (define parsed (parse-token-with-position token))
       (if parsed
           (loop rest (cons parsed result))
           (loop rest result))])))

;; Parse individual token with position
(define (parse-token-with-position token)
  (match token
    [(positioned-token value pos)
     (cond
       [(string=? value "") #f]
       [(string-prefix? value "\"") (parse-string value)]
       [(string-prefix? value "@") (parse-sigil value)]
       [(string->number value) => (lambda (x) x)]
       [else (string->symbol value)])]))

;; Parse definition with positions
(define (parse-definition-with-positions tokens)
  (match tokens
    [(cons (positioned-token name name-pos) rest)
     (let loop ([tokens rest] [body '()])
       (match tokens
         [(cons (positioned-token ";" _) rest)
          (values (list 'define (string->symbol name) (reverse body)) rest)]
         [(cons token rest)
          (define parsed (parse-token-with-position token))
          (if parsed
              (loop rest (cons parsed body))
              (loop rest body))]
         ['()
          (define error-msg "Unterminated word definition - missing ';'")
          (define error (syntax-error error-msg name-pos 
                                     (list "Add ';' at the end of the word definition"
                                           "Check for balanced brackets and quotes")))
          (error (format-error-with-context error ""))]))]
    ['()
     (define error-msg "Empty word definition after ':'")
     (define error (syntax-error error-msg (make-source-position 1 1)
                                (list "Provide a word name after ':'"
                                      "Example: : my-word ... ;")))
     (error (format-error-with-context error ""))]))

;; Parse map with positions  
(define (parse-map-with-positions tokens)
  (let loop ([tokens tokens] [pairs '()])
    (match tokens
      [(cons (positioned-token "}" _) rest)
       (values (make-hash pairs) rest)]
      [(cons key-token (cons value-token rest))
       (define key (parse-token-with-position key-token))
       (define value (parse-token-with-position value-token))
       (loop rest (cons (cons key value) pairs))]
      ['()
       (define error-msg "Unterminated map - missing '}'")
       (define error (syntax-error error-msg (make-source-position 1 1)
                                  (list "Add '}' to close the map"
                                        "Check for balanced braces")))
       (error (format-error-with-context error ""))])))

;; Parse list with positions
(define (parse-list-with-positions tokens)
  (let loop ([tokens tokens] [items '()])
    (match tokens
      [(cons (positioned-token "]" _) rest)
       (values (reverse items) rest)]
      [(cons token rest)
       (define parsed (parse-token-with-position token))
       (if parsed
           (loop rest (cons parsed items))
           (loop rest items))]
      ['()
       (define error-msg "Unterminated list - missing ']'")
       (define error (syntax-error error-msg (make-source-position 1 1)
                                  (list "Add ']' to close the list"
                                        "Check for balanced brackets")))
       (error (format-error-with-context error ""))])))

;; Parse string (same as original)
(define (parse-string token)
  (if (and (string-prefix? token "\"") (string-suffix? token "\""))
      (substring token 1 (- (string-length token) 1))
      token))

;; Parse sigil (same as original) 
(define (parse-sigil token)
  (cond
    [(string=? token "@webhook.listen") 'webhook-listen]
    [(string-prefix? token "@") (string->symbol token)]
    [else (string->symbol token)]))