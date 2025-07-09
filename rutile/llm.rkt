#lang racket/base

(require racket/hash)
(require racket/match)
(require racket/file)
(require net/base64)
(require "http.rkt")

(provide llm-builtin?
         execute-llm-builtin
         make-llm-context)

;; LLM context management
(define (make-llm-context)
  (make-hash))

;; Check if a word is an LLM builtin
(define (llm-builtin? name)
  (member name '("llm-call" "llm-default-config" "llm-stream" "llm-multi-turn"
                 "llm-set-model" "llm-set-config" "llm-with-model" "llm-with-config" "llm-context"
                 "llm-image" "llm-json-schema" "llm-function" "llm-tools" "llm-reasoning")))

;; Execute LLM builtin operations
(define (execute-llm-builtin vm name vm-pop vm-push vm-stack vm-llm-context)
  (match name
    ["llm-call"
     ;; LLM call with context support
     (cond
       [(= (length (vm-stack vm)) 1)
        ;; Just prompt - use default context
        (define prompt (vm-pop vm))
        (define base-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
        (define model (hash-ref (vm-llm-context vm) 'default-model "gpt-4o"))
        (define config (apply hash 'model model 
                             (if (hash-empty? base-config)
                                 '()
                                 (apply append (map (lambda (p) (list (car p) (cdr p))) 
                                                   (hash->list base-config))))))
        (define response (openai-responses-call prompt model config))
        (vm-push vm response)]
       [else
        ;; Traditional config + prompt  
        (define prompt (vm-pop vm))
        (define config (vm-pop vm))
        (define base-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
        (define merged-config (apply hash (append (apply append (map (lambda (p) (list (car p) (cdr p))) (hash->list base-config)))
                                                           (apply append (map (lambda (p) (list (car p) (cdr p))) (hash->list config))))))
        (define model (hash-ref merged-config 'model "gpt-4o"))
        (define response (openai-responses-call prompt model merged-config))
        (vm-push vm response)])]
    ["llm-default-config"
     (define config (hash 'model "gpt-4o" 'temperature 0.7))
     (vm-push vm config)]
    ["llm-stream"
     (define config (vm-pop vm))
     (define prompt (vm-pop vm))
     (define model (hash-ref config 'model "gpt-4o"))
     (define response (openai-responses-stream prompt model config))
     (vm-push vm response)]
    ["llm-multi-turn"
     (define previous-id (vm-pop vm))
     (define config (vm-pop vm))
     (define prompt (vm-pop vm))
     (define model (hash-ref config 'model "gpt-4o"))
     (define response (openai-responses-multi-turn prompt model config previous-id))
     (vm-push vm response)]
    ["llm-set-model"
     ;; Set default model: "gpt-4o" llm-set-model
     (define model (vm-pop vm))
     (hash-set! (vm-llm-context vm) 'default-model model)
     (displayln (format "Default LLM model set to: ~a" model))]
    ["llm-set-config"
     ;; Set default config: {temperature: 0.8} llm-set-config
     (define config (vm-pop vm))
     (hash-set! (vm-llm-context vm) 'default-config config)
     (displayln "Default LLM config updated")]
    ["llm-with-model"
     ;; Use model temporarily: "prompt" "o3-mini" llm-with-model
     (define model (vm-pop vm))
     (define prompt (vm-pop vm))
     (define base-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
     (define config (apply hash 'model model 
                          (if (hash-empty? base-config)
                              '()
                              (apply append (map (lambda (p) (list (car p) (cdr p))) 
                                                (hash->list base-config))))))
     (define response (openai-responses-call prompt model config))
     (vm-push vm response)]
    ["llm-with-config"
     ;; Use config temporarily: "prompt" {temperature: 0.2} llm-with-config
     (define config (vm-pop vm))
     (define prompt (vm-pop vm))
     (define base-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
     (define merged-config (apply hash (append (apply append (map (lambda (p) (list (car p) (cdr p))) (hash->list base-config)))
                                                       (apply append (map (lambda (p) (list (car p) (cdr p))) (hash->list config))))))
     (define model (hash-ref merged-config 'model "gpt-4o"))
     (define response (openai-responses-call prompt model merged-config))
     (vm-push vm response)]
    ["llm-context"
     ;; Show current LLM context
     (vm-push vm (vm-llm-context vm))]
    ["llm-image"
     ;; Image input: "image.png" "What's in this image?" llm-image
     (define prompt (vm-pop vm))
     (define image-path (vm-pop vm))
     (define base-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
     (define config (hash-set base-config 'model "gpt-4o"))  ;; Force multimodal model
     ;; Create message with image input for Responses API
     (define message-content 
       (list (hash 'type "text" 'text prompt)
             (hash 'type "image_url" 
                   'image_url (hash 'url (format "data:image/png;base64,~a" (file->base64 image-path))))))
     (define input (list (hash 'type "message" 
                               'message (hash 'role "user" 'content message-content))))
     (define response (openai-responses-call-with-input input config))
     (vm-push vm response)]
    ["llm-json-schema"
     ;; Structured output: {schema: {...}} "prompt" llm-json-schema
     (define prompt (vm-pop vm))
     (define schema (vm-pop vm))
     (define base-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
     (define config (hash-set* base-config 
                               'response_format (hash 'type "json_schema" 'json_schema schema)))
     (define model (hash-ref config 'model "gpt-4o"))
     (define response (openai-responses-call prompt model config))
     (vm-push vm response)]
    ["llm-function"
     ;; Function calling setup: [{function spec}] llm-function
     (define functions (vm-pop vm))
     (define current-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
     (define tools (map (lambda (f) (hash 'type "function" 'function f)) functions))
     (define config-pairs (if (hash-empty? current-config)
                              (list (cons 'tools tools))
                              (cons (cons 'tools tools) (hash->list current-config))))
     (define updated-config (apply hash (apply append (map (lambda (p) (list (car p) (cdr p))) config-pairs))))
     (hash-set! (vm-llm-context vm) 'default-config updated-config)
     (displayln "Function tools registered")]
    ["llm-tools"
     ;; Built-in tools: ["web_search" "code_interpreter"] llm-tools  
     (define tool-names (vm-pop vm))
     (define current-config (hash-ref (vm-llm-context vm) 'default-config (hash)))
     (define tools (map (lambda (name) (hash 'type name)) tool-names))
     (define config-pairs (if (hash-empty? current-config)
                              (list (cons 'tools tools))
                              (cons (cons 'tools tools) (hash->list current-config))))
     (define updated-config (apply hash (apply append (map (lambda (p) (list (car p) (cdr p))) config-pairs))))
     (hash-set! (vm-llm-context vm) 'default-config updated-config)
     (displayln (format "Built-in tools enabled: ~a" tool-names))]
    ["llm-reasoning"
     ;; Enable reasoning mode: "prompt" llm-reasoning
     (define prompt (vm-pop vm))
     ;; Reasoning models don't support temperature, max_tokens, etc.
     (define config (hash 'model "o3-mini"))
     (define response (openai-responses-call prompt "o3-mini" config))
     (vm-push vm response)]
    [else (error "Unknown LLM builtin:" name)]))

;; Helper functions for LLM features
(define (file->base64 filepath)
  "Convert file to base64 string"
  (with-handlers ([exn:fail? (lambda (e) "")])
    (base64-encode (file->bytes filepath))))