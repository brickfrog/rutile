#lang racket/base

(require "vm.rkt")
(require "testing.rkt")

(provide initialize-testing-stdlib)

(define (initialize-testing-stdlib vm)
  ;; Add generator functions
  (vm-define-word vm "gen-integer" 
    '(lambda ()
       (define max-val (vm-pop vm))
       (define min-val (vm-pop vm))
       (define gen (gen-integer #:min min-val #:max max-val))
       (vm-push vm gen)))
  
  (vm-define-word vm "gen-string"
    '(lambda ()
       (define max-len (vm-pop vm))
       (define gen (gen-string #:max-length max-len))
       (vm-push vm gen)))
  
  (vm-define-word vm "gen-boolean"
    '(lambda ()
       (define gen (gen-boolean))
       (vm-push vm gen)))
  
  (vm-define-word vm "gen-list"
    '(lambda ()
       (define max-len (vm-pop vm))
       (define element-gen (vm-pop vm))
       (define gen (gen-list element-gen #:max-length max-len))
       (vm-push vm gen)))
  
  (vm-define-word vm "sample"
    '(lambda ()
       (define count (vm-pop vm))
       (define gen (vm-pop vm))
       (define samples (sample gen #:count count))
       (vm-push vm samples)))
  
  (vm-define-word vm "forall"
    '(lambda ()
       (define property (vm-pop vm))
       (define gen (vm-pop vm))
       (define test-fn (forall gen (lambda (input)
                                    (vm-push vm input)
                                    (execute-tokens vm property)
                                    (vm-pop vm))))
       (vm-push vm test-fn)))
  
  (vm-define-word vm "quick-check"
    '(lambda ()
       (define iterations (vm-pop vm))
       (define property (vm-pop vm))
       (define result (quick-check property #:iterations iterations))
       (vm-push vm result))))