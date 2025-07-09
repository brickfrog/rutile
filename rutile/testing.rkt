#lang racket/base

(require racket/random)
(require racket/list)
(require racket/string)
(require racket/match)
(require "vm.rkt")
(require "parser.rkt")

(provide property-test
         gen-integer
         gen-float
         gen-string
         gen-boolean
         gen-list
         gen-quotation
         gen-map
         forall
         such-that
         sample
         shrink
         test-runner
         quick-check
         property-based-test-suite)

;; Generator data structure
(struct generator (fn) #:transparent)

;; Property test result
(struct test-result (passed? input output error iterations) #:transparent)

;; Basic generators
(define (gen-integer #:min [min-val -1000] #:max [max-val 1000])
  (generator (lambda () (random min-val max-val))))

(define (gen-float #:min [min-val -1000.0] #:max [max-val 1000.0])
  (generator (lambda () (+ min-val (* (random) (- max-val min-val))))))

(define (gen-boolean)
  (generator (lambda () (random-ref '(#t #f)))))

(define (gen-string #:max-length [max-len 20])
  (generator (lambda ()
    (define len (random 0 (+ max-len 1)))
    (apply string (for/list ([i (in-range len)])
                    (integer->char (random 32 127)))))))

(define (gen-char)
  (generator (lambda () (integer->char (random 32 127)))))

(define (gen-list gen #:max-length [max-len 10])
  (generator (lambda ()
    (define len (random 0 (+ max-len 1)))
    (for/list ([i (in-range len)])
      ((generator-fn gen))))))

(define (gen-quotation #:max-length [max-len 5])
  (generator (lambda ()
    (define len (random 0 (+ max-len 1)))
    (for/list ([i (in-range len)])
      (case (random 4)
        [(0) ((generator-fn (gen-integer)))]
        [(1) ((generator-fn (gen-string #:max-length 10)))]
        [(2) ((generator-fn (gen-boolean)))]
        [(3) 'dup])))))

(define (gen-map #:max-size [max-size 5])
  (generator (lambda ()
    (define size (random 0 (+ max-size 1)))
    (define h (make-hash))
    (for ([i (in-range size)])
      (hash-set! h 
                 ((generator-fn (gen-string #:max-length 10)))
                 ((generator-fn (gen-integer)))))
    h)))

(define (gen-one-of . generators)
  (generator (lambda ()
    (define chosen (random-ref generators))
    ((generator-fn chosen)))))

(define (gen-frequency . freq-gen-pairs)
  (generator (lambda ()
    (define total-weight (apply + (map car freq-gen-pairs)))
    (define r (random total-weight))
    (let loop ([pairs freq-gen-pairs] [acc 0])
      (match pairs
        ['() (error "gen-frequency: empty list")]
        [(cons (list weight gen) rest)
         (define new-acc (+ acc weight))
         (if (< r new-acc)
             ((generator-fn gen))
             (loop rest new-acc))])))))

;; Property combinators
(define (forall gen property)
  (lambda () 
    (define input ((generator-fn gen)))
    (property input)))

(define (such-that gen predicate #:max-tries [max-tries 100])
  (generator (lambda ()
    (let loop ([tries 0])
      (if (>= tries max-tries)
          (error "such-that: gave up after ~a tries" max-tries)
          (let ([value ((generator-fn gen))])
            (if (predicate value)
                value
                (loop (+ tries 1)))))))))

;; Sampling and shrinking
(define (sample gen #:count [count 10])
  (for/list ([i (in-range count)])
    ((generator-fn gen))))

(define (shrink value)
  (cond
    [(integer? value) 
     (cond
       [(= value 0) '()]
       [(> value 0) (list (quotient value 2) (- value 1))]
       [(< value 0) (list (quotient value 2) (+ value 1))])]
    [(string? value)
     (if (= (string-length value) 0)
         '()
         (list (substring value 0 (quotient (string-length value) 2))))]
    [(list? value)
     (if (null? value)
         '()
         (cons (cdr value) 
               (map (lambda (x) (cons x (cdr value))) 
                    (shrink (car value)))))]
    [else '()]))

;; Property testing runner
(define (property-test property #:iterations [iterations 100] #:verbose [verbose #f])
  (let loop ([i 0] [failures '()])
    (cond
      [(>= i iterations)
       (if (null? failures)
           (test-result #t #f #f #f iterations)
           (test-result #f 
                       (map car failures)
                       (map cdr failures)
                       #f
                       iterations))]
      [else
       (with-handlers ([exn:fail? (lambda (e)
                                   (when verbose
                                     (displayln (format "Test ~a failed: ~a" i (exn-message e))))
                                   (loop (+ i 1) (cons (cons i (exn-message e)) failures)))])
         (define result (property))
         (if result
             (begin
               (when verbose
                 (displayln (format "Test ~a passed" i)))
               (loop (+ i 1) failures))
             (begin
               (when verbose
                 (displayln (format "Test ~a failed: property returned false" i)))
               (loop (+ i 1) (cons (cons i "property returned false") failures)))))])))

;; Quick check style interface
(define (quick-check property #:iterations [iterations 100])
  (define result (property-test property #:iterations iterations))
  (if (test-result-passed? result)
      (begin
        (displayln (format "All ~a tests passed" iterations))
        #t)
      (begin
        (displayln (format "Tests failed after ~a iterations" iterations))
        (for ([failure (test-result-input result)])
          (displayln (format "  Failure: ~a" failure)))
        #f)))

;; Rutile-specific generators
(define (gen-rutile-word)
  (gen-one-of (gen-string #:max-length 10)
              (generator (lambda () 'dup))
              (generator (lambda () '+))
              (generator (lambda () 'swap))))

(define (gen-rutile-stack)
  (gen-list (gen-one-of (gen-integer) (gen-string #:max-length 10) (gen-boolean))))

;; VM property tests
(define (test-stack-operations)
  (forall (gen-list (gen-integer) #:max-length 5)
    (lambda (initial-stack)
      (define vm (make-vm))
      ;; Push all elements
      (for ([elem initial-stack])
        (vm-push vm elem))
      ;; Check depth
      (vm-push vm 'depth)
      (execute-builtin vm "depth")
      (define depth (vm-pop vm))
      (= depth (length initial-stack)))))

(define (test-dup-operation)
  (forall (gen-integer)
    (lambda (value)
      (define vm (make-vm))
      (vm-push vm value)
      (execute-builtin vm "dup")
      (define val1 (vm-pop vm))
      (define val2 (vm-pop vm))
      (and (= val1 value) (= val2 value)))))

(define (test-swap-operation)
  (forall (gen-list (gen-integer) #:max-length 2)
    (lambda (values)
      (if (< (length values) 2)
          #t  ;; Skip test for insufficient values
          (let ([a (first values)]
                [b (second values)])
            (define vm (make-vm))
            (vm-push vm a)
            (vm-push vm b)
            (execute-builtin vm "swap")
            (define swapped-b (vm-pop vm))
            (define swapped-a (vm-pop vm))
            (and (= swapped-a a) (= swapped-b b)))))))

(define (test-arithmetic-operations)
  (forall (gen-list (gen-integer #:min 1 #:max 100) #:max-length 2)
    (lambda (values)
      (if (< (length values) 2)
          #t
          (let ([a (first values)]
                [b (second values)])
            (define vm (make-vm))
            (vm-push vm a)
            (vm-push vm b)
            (execute-builtin vm "+")
            (define result (vm-pop vm))
            (= result (+ a b)))))))

(define (test-string-operations)
  (forall (gen-list (gen-string #:max-length 10) #:max-length 2)
    (lambda (strings)
      (if (< (length strings) 2)
          #t
          (let ([a (first strings)]
                [b (second strings)])
            (define vm (make-vm))
            (vm-push vm a)
            (vm-push vm b)
            (execute-builtin vm "concat")
            (define result (vm-pop vm))
            (string=? result (string-append a b)))))))

(define (test-list-operations)
  (forall (gen-list (gen-integer) #:max-length 5)
    (lambda (lst)
      (define vm (make-vm))
      (vm-push vm lst)
      (execute-builtin vm "length")
      (define result (vm-pop vm))
      (= result (length lst)))))

;; Test runner
(define (test-runner tests #:verbose [verbose #f])
  (define passed 0)
  (define failed 0)
  
  (for ([test-pair tests])
    (define name (car test-pair))
    (define test-fn (cdr test-pair))
    
    (displayln (format "Running test: ~a" name))
    
    (define result (property-test test-fn #:verbose verbose))
    
    (if (test-result-passed? result)
        (begin
          (displayln (format "  ~a passed (~a iterations)" name (test-result-iterations result)))
          (set! passed (+ passed 1)))
        (begin
          (displayln (format "  ~a failed" name))
          (when (test-result-input result)
            (displayln (format "    Failures: ~a" (test-result-input result))))
          (set! failed (+ failed 1)))))
  
  (displayln (format "\nTest Summary: ~a passed, ~a failed" passed failed))
  (= failed 0))

;; Complete property-based test suite
(define (property-based-test-suite)
  (define tests
    (list (cons "stack-operations" test-stack-operations)
          (cons "dup-operation" test-dup-operation)
          (cons "swap-operation" test-swap-operation)
          (cons "arithmetic-operations" test-arithmetic-operations)
          (cons "string-operations" test-string-operations)
          (cons "list-operations" test-list-operations)))
  
  (displayln "=== Rutile Property-Based Test Suite ===")
  (test-runner tests))