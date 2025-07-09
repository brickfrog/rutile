#lang racket

(require racket/string)
(require racket/file)

(provide rutile-error
         syntax-error
         runtime-error
         make-source-position
         format-error-with-context
         suggest-corrections
         suggest-unknown-word
         get-line-content)

;; Source position tracking
(struct source-position (line column file) #:transparent)

;; Error types
(struct rutile-error (message type position suggestions) #:transparent)

;; Create source position
(define (make-source-position line column [file #f])
  (source-position line column file))

;; Create syntax error with position
(define (syntax-error message position [suggestions '()])
  (rutile-error message 'syntax position suggestions))

;; Create runtime error with position
(define (runtime-error message position [suggestions '()])
  (rutile-error message 'runtime position suggestions))

;; Format error with source context
(define (format-error-with-context error source-text)
  (match error
    [(rutile-error message type position suggestions)
     (define formatted-message
       (if position
           (format-error-with-position message type position source-text)
           (format "~a error: ~a" (symbol->string type) message)))
     
     (define suggestion-text
       (if (null? suggestions)
           ""
           (format "\n\nSuggestions:\n~a"
                   (string-join (map (lambda (s) (format "  • ~a" s)) suggestions) "\n"))))
     
     (string-append formatted-message suggestion-text)]
    
    [_ (format "Error: ~a" error)]))

;; Format error with position information
(define (format-error-with-position message type position source-text)
  (match position
    [(source-position line column file)
     (define file-info (if file (format " in ~a" file) ""))
     (define location (format "line ~a, column ~a~a" line column file-info))
     
     (define context (get-error-context line column source-text))
     
     (format "~a error at ~a:\n~a\n\n~a"
             (string->title-case (symbol->string type))
             location
             message
             context)]
    
    [_ (format "~a error: ~a" (symbol->string type) message)]))

;; Get error context with line highlighting
(define (get-error-context line column source-text)
  (define lines (string-split source-text "\n"))
  (define line-idx (- line 1))
  
  (if (and (>= line-idx 0) (< line-idx (length lines)))
      (let* ([error-line (list-ref lines line-idx)]
             [line-num-width (string-length (number->string (+ line 2)))]
             [context-lines (get-context-lines lines line-idx)]
             [pointer-line (make-error-pointer column line-num-width)])
        
        (string-append
         (format-context-lines context-lines (- line 1) line-num-width)
         "\n"
         pointer-line))
      
      ""))

;; Get surrounding context lines
(define (get-context-lines lines error-line-idx)
  (define start-idx (max 0 (- error-line-idx 2)))
  (define end-idx (min (length lines) (+ error-line-idx 3)))
  
  (for/list ([i (in-range start-idx end-idx)])
    (cons (+ i 1) (list-ref lines i))))

;; Format context lines with line numbers
(define (format-context-lines context-lines error-line line-num-width)
  (string-join
   (for/list ([line-info context-lines])
     (match line-info
       [(cons line-num content)
        (define prefix (if (= line-num error-line) ">" " "))
        (define num-str (format "~a~a"
                               (~a line-num #:width line-num-width #:align 'right)
                               prefix))
        (format "~a │ ~a" num-str content)]))
   "\n"))

;; Create error pointer line
(define (make-error-pointer column line-num-width)
  (define spaces (+ line-num-width 3 (- column 1)))
  (format "~a │ ~a^"
          (make-string line-num-width #\space)
          (make-string spaces #\space)))

;; Get content of specific line
(define (get-line-content source-text line-num)
  (define lines (string-split source-text "\n"))
  (define line-idx (- line-num 1))
  
  (if (and (>= line-idx 0) (< line-idx (length lines)))
      (list-ref lines line-idx)
      ""))

;; Suggest corrections for common errors
(define (suggest-corrections error-message available-words)
  (cond
    [(string-contains? error-message "Unknown word:")
     (suggest-unknown-word error-message available-words)]
    
    [(string-contains? error-message "Stack underflow")
     (list "Check that you have enough values on the stack"
           "Use 'depth' to check stack size"
           "Make sure all operations have their required arguments")]
    
    [(string-contains? error-message "not a number")
     (list "Check that the value is actually a number"
           "Use 'number?' to test if a value is numeric"
           "Convert strings to numbers with appropriate operations")]
    
    [(string-contains? error-message "not a string")
     (list "Check that the value is actually a string"
           "Use 'string?' to test if a value is a string"
           "Convert values to strings with string conversion operations")]
    
    [else '()]))

;; Suggest corrections for unknown words
(define (suggest-unknown-word error-message available-words)
  (define word-match (regexp-match #rx"Unknown word: (.+)" error-message))
  (if word-match
      (let ([unknown-word (cadr word-match)])
        (define suggestions (find-similar-words unknown-word available-words))
        (if (null? suggestions)
            (list "Check spelling of word name"
                  "Make sure the word is defined before use"
                  "Use ':words' to see available words")
            (cons (format "Did you mean: ~a" (string-join suggestions ", "))
                  (list "Check spelling of word name"
                        "Use ':words' to see all available words"))))
      '()))

;; Find similar words using edit distance
(define (find-similar-words target words)
  (define max-distance 2)
  (define candidates
    (filter (lambda (word)
              (<= (edit-distance target word) max-distance))
            words))
  
  (take candidates (min 3 (length candidates))))

;; Edit distance calculation for word suggestions
(define (edit-distance s1 s2)
  (define len1 (string-length s1))
  (define len2 (string-length s2))
  
  (cond
    [(= len1 0) len2]
    [(= len2 0) len1]
    [else
     (define matrix (make-vector (* (+ len1 1) (+ len2 1)) 0))
     
     ;; Initialize first row and column
     (for ([i (in-range (+ len1 1))])
       (vector-set! matrix i i))
     (for ([j (in-range (+ len2 1))])
       (vector-set! matrix (* j (+ len1 1)) j))
     
     ;; Fill matrix
     (for* ([i (in-range 1 (+ len1 1))]
            [j (in-range 1 (+ len2 1))])
       (define cost (if (char=? (string-ref s1 (- i 1))
                               (string-ref s2 (- j 1))) 0 1))
       (define idx (+ (* j (+ len1 1)) i))
       (vector-set! matrix idx
                   (min (+ (vector-ref matrix (- idx 1)) 1)          ; insertion
                        (+ (vector-ref matrix (- idx (+ len1 1))) 1)  ; deletion
                        (+ (vector-ref matrix (- idx (+ len1 2))) cost)))) ; substitution
     
     (vector-ref matrix (- (* (+ len1 1) (+ len2 1)) 1))]))

;; String title case helper
(define (string->title-case str)
  (if (> (string-length str) 0)
      (string-append (string-upcase (substring str 0 1))
                     (string-downcase (substring str 1)))
      str))