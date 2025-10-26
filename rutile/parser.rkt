#lang racket/base

(require racket/string)
(require racket/match)
(require json)
(require racket/list)

(provide parse-rutile tokenize)

(define (parse-rutile input)
  (define tokens (tokenize input))
  (parse-tokens tokens))

(define (tokenize input)
  (define clean-input (regexp-replace* #rx"#[^\n]*" input ""))
  (define no-paren-comments (remove-paren-comments clean-input))
  (define tokens (tokenize-with-strings (string-trim no-paren-comments)))
  (filter (lambda (s) (not (string=? s ""))) tokens))

(define (tokenize-with-strings input)
  (define result '())
  (define current-token "")
  (define in-string? #f)
  (define in-multiline? #f)
  (define chars (string->list input))

  (let loop ([chars chars])
    (when (not (null? chars))
      (define char (car chars))
      (define rest (cdr chars))

      (cond
        ;; Check for triple-quote start/end
        [(and (char=? char #\")
              (>= (length chars) 3)
              (char=? (list-ref chars 1) #\")
              (char=? (list-ref chars 2) #\"))
         (if in-multiline?
             ;; End multiline
             (begin
               (set! current-token (string-append current-token "\"\"\""))
               (set! result (cons current-token result))
               (set! current-token "")
               (set! in-multiline? #f)
               (loop (cdddr chars)))
             ;; Start multiline
             (begin
               (when (not (string=? current-token ""))
                 (set! result (cons current-token result))
                 (set! current-token ""))
               (set! current-token "\"\"\"")
               (set! in-multiline? #t)
               (loop (cdddr chars))))]

        ;; Regular quote (not triple)
        [(char=? char #\")
         (if (and (not in-multiline?) in-string?)
             (begin
               (set! current-token (string-append current-token "\""))
               (set! result (cons current-token result))
               (set! current-token "")
               (set! in-string? #f)
               (loop rest))
             (if (not in-multiline?)
                 (begin
                   (when (not (string=? current-token ""))
                     (set! result (cons current-token result))
                     (set! current-token ""))
                   (set! current-token "\"")
                   (set! in-string? #t)
                   (loop rest))
                 (begin
                   (set! current-token (string-append current-token (string char)))
                   (loop rest))))]

        ;; Inside any string (regular or multiline)
        [(or in-string? in-multiline?)
         (set! current-token (string-append current-token (string char)))
         (loop rest)]

        ;; Brackets and braces
        [(or (char=? char #\[) (char=? char #\]) (char=? char #\{) (char=? char #\}))
         (when (not (string=? current-token ""))
           (set! result (cons current-token result))
           (set! current-token ""))
         (set! result (cons (string char) result))
         (loop rest)]

        ;; Whitespace
        [(char-whitespace? char)
         (when (not (string=? current-token ""))
           (set! result (cons current-token result))
           (set! current-token ""))
         (loop rest)]

        ;; Regular character
        [else
         (set! current-token (string-append current-token (string char)))
         (loop rest)])))

  (when (not (string=? current-token ""))
    (set! result (cons current-token result)))

  (reverse result))

(define (remove-paren-comments input)
  (regexp-replace* #rx"\\([^)]*\\)" input ""))

(define (parse-tokens tokens)
  (let loop ([tokens tokens] [result '()])
    (match tokens
      ['() (reverse result)]
      [(cons ":" rest)
       (let-values ([(def-token remaining) (parse-definition rest)])
         (loop remaining (cons def-token result)))]
      [(cons "{" rest)
       (let-values ([(map-token remaining) (parse-map rest)])
         (loop remaining (cons map-token result)))]
      [(cons "[" rest)
       (let-values ([(list-token remaining) (parse-list rest)])
         (loop remaining (cons list-token result)))]
      [(cons token rest)
       (define parsed (parse-token token))
       (if parsed
           (loop rest (cons parsed result))
           (loop rest result))])))

(define (parse-token token)
  (cond
    [(string=? token "") #f]
    [(string-prefix? token "\"") (parse-string token)]
    [(string-prefix? token "@") (parse-sigil token)]
    [(string->number token) => (lambda (x) x)]
    [else (string->symbol token)]))

(define (parse-sigil token)
  (cond
    [(string=? token "@webhook.listen") 'webhook-listen]
    [(string-prefix? token "@") (string->symbol token)]  ; Keep the @ symbol
    [else (string->symbol token)]))

(define (parse-definition tokens)
  (match tokens
    [(cons name rest)
     (let loop ([tokens rest] [body '()])
       (match tokens
         ['() (error "Unterminated definition")]
         [(cons ";" rest)
          (values (list 'define-word name (reverse body)) rest)]
         [(cons token rest)
          (define parsed (parse-token token))
          (if parsed
              (loop rest (cons parsed body))
              (loop rest body))]))]))

(define (parse-map tokens)
  (let loop ([tokens tokens] [pairs '()])
    (match tokens
      ['() (error "Unterminated map")]
      [(cons "}" rest)
       (values (make-hash pairs) rest)]
      [(cons key-token (cons value-token rest))
       (define key (parse-token key-token))
       (define value (parse-token value-token))
       (loop rest (cons (cons key value) pairs))])))

(define (parse-list tokens)
  (let loop ([tokens tokens] [items '()])
    (match tokens
      ['() (error "Unterminated list")]
      [(cons "]" rest)
       (values (reverse items) rest)]
      [(cons token rest)
       (define parsed (parse-token token))
       (if parsed
           (loop rest (cons parsed items))
           (loop rest items))])))

(define (parse-string token)
  (cond
    ;; Triple-quoted multiline string
    [(and (string-prefix? token "\"\"\"") (string-suffix? token "\"\"\""))
     (substring token 3 (- (string-length token) 3))]
    ;; Regular string
    [(and (string-prefix? token "\"") (string-suffix? token "\""))
     (substring token 1 (- (string-length token) 1))]
    [else token]))

