#lang racket/base

(require racket/match)
(require net/http-client)
(require json)
(require "vm.rkt")
(require "events.rkt")
(require "hotreload.rkt")

(provide initialize-stdlib)

(define (initialize-stdlib vm)
  (init-event-sources)
  (init-hot-reload)
  (define-stdlib-words vm))

(define (define-stdlib-words vm)
  ;; Stack inspection words  
  (vm-define-word vm "print" '(print-top))
  (vm-define-word vm "clear" '(clear-stack))
  (vm-define-word vm "words" '(show-words))
  (vm-define-word vm "stack" '(show-stack))
  
  ;; HTTP words
  (vm-define-word vm "http.get" '(http-get))
  (vm-define-word vm "http.post" '(http-post))
  
  ;; JSON words
  (vm-define-word vm "json.parse" '(json-parse))
  (vm-define-word vm "json.stringify" '(json-stringify))
  
  ;; LLM words (placeholder)
  (vm-define-word vm "llm.call" '(llm-call))
  (vm-define-word vm "llm.default_cfg" '(llm-default-config)))