#lang racket/base

(require net/http-client)
(require json)
(require racket/port)
(require racket/string)
(require racket/date)

(provide openai-responses-call
         openai-responses-stream
         openai-responses-multi-turn
         openai-responses-call-with-input)

(define (openai-responses-call input model config)
  (define api-key (getenv "OPENAI_API_KEY"))
  (unless api-key
    (error "OPENAI_API_KEY environment variable not set"))
  
  (define base-request
    (if (hash-has-key? config 'temperature)
        (hash 'model model
              'input input
              'temperature (hash-ref config 'temperature)
              'stream #f)
        (hash 'model model
              'input input
              'stream #f)))
  
  ;; Add all supported config options
  (define request-body
    (foldl (lambda (key acc)
             (if (hash-has-key? config key)
                 (hash-set acc key (hash-ref config key))
                 acc))
           base-request
           '(max_output_tokens max_tool_calls tools response_format
             parallel_tool_calls previous_response_id instructions
             metadata include)))
  
  (define json-body (jsexpr->string request-body))
  
  (define-values (status headers response-port)
    (http-sendrecv "api.openai.com"
                   "/v1/responses"
                   #:ssl? #t
                   #:method "POST"
                   #:headers (list (format "Authorization: Bearer ~a" api-key)
                                  "Content-Type: application/json")
                   #:data json-body))
  
  (define response-body (port->string response-port))
  
  (unless (bytes=? (subbytes status 9 12) #"200")
    (error (format "OpenAI API error: ~a - ~a" status response-body)))
  
  (define response-json (string->jsexpr response-body))
  
  ;; Extract the text from the response
  (define output (hash-ref response-json 'output '()))
  (if (null? output)
      ""
      (let* ([first-output (car output)]
             [content (hash-ref first-output 'content '())])
        (if (null? content)
            ""
            (let ([first-content (car content)])
              (hash-ref first-content 'text ""))))))

(define (openai-responses-stream input model config)
  (define api-key (getenv "OPENAI_API_KEY"))
  (unless api-key
    (error "OPENAI_API_KEY environment variable not set"))
  
  (define base-request
    (hash 'model model
          'input input
          'temperature (hash-ref config 'temperature 0.7)
          'stream #t))
  
  (define request-body
    (if (hash-has-key? config 'max_output_tokens)
        (hash-set base-request 'max_output_tokens (hash-ref config 'max_output_tokens))
        base-request))
  
  (define json-body (jsexpr->string request-body))
  
  (define-values (status headers response-port)
    (http-sendrecv "api.openai.com"
                   "/v1/responses"
                   #:ssl? #t
                   #:method "POST"
                   #:headers (list (format "Authorization: Bearer ~a" api-key)
                                  "Content-Type: application/json")
                   #:data json-body))
  
  (unless (bytes=? (subbytes status 9 12) #"200")
    (error (format "OpenAI API error: ~a" status)))
  
  ;; Stream the response
  (let loop ([result ""])
    (define line (read-line response-port))
    (if (eof-object? line)
        result
        (if (string-prefix? line "data: ")
            (let ([data (substring line 6)])
              (if (string=? data "[DONE]")
                  result
                  (with-handlers ([exn:fail? (lambda (e) result)])
                    (define event (string->jsexpr data))
                    (define type (hash-ref event 'type ""))
                    (if (string=? type "response.output_text.delta")
                        (let ([delta (hash-ref event 'delta "")])
                          (display delta)
                          (flush-output)
                          (loop (string-append result delta)))
                        (loop result)))))
            (loop result)))))

(define (openai-responses-multi-turn input model config previous-response-id)
  (define api-key (getenv "OPENAI_API_KEY"))
  (unless api-key
    (error "OPENAI_API_KEY environment variable not set"))
  
  (define base-request
    (hash 'model model
          'input input
          'temperature (hash-ref config 'temperature 0.7)
          'stream #f))
  
  (define with-previous
    (if previous-response-id
        (hash-set base-request 'previous_response_id previous-response-id)
        base-request))
  
  (define request-body
    (if (hash-has-key? config 'max_output_tokens)
        (hash-set with-previous 'max_output_tokens (hash-ref config 'max_output_tokens))
        with-previous))
  
  (define json-body (jsexpr->string request-body))
  
  (define-values (status headers response-port)
    (http-sendrecv "api.openai.com"
                   "/v1/responses"
                   #:ssl? #t
                   #:method "POST"
                   #:headers (list (format "Authorization: Bearer ~a" api-key)
                                  "Content-Type: application/json")
                   #:data json-body))
  
  (define response-body (port->string response-port))
  
  (unless (bytes=? (subbytes status 9 12) #"200")
    (error (format "OpenAI API error: ~a - ~a" status response-body)))
  
  (define response-json (string->jsexpr response-body))
  
  ;; Return both the response text and the response ID for chaining
  (define response-id (hash-ref response-json 'id))
  (define output (hash-ref response-json 'output '()))
  (define response-text
    (if (null? output)
        ""
        (let* ([first-output (car output)]
               [content (hash-ref first-output 'content '())])
          (if (null? content)
              ""
              (let ([first-content (car content)])
                (hash-ref first-content 'text ""))))))
  
  (hash 'text response-text 'id response-id))

;; Enhanced function for structured input (images, etc.)
(define (openai-responses-call-with-input input config)
  (define api-key (getenv "OPENAI_API_KEY"))
  (unless api-key
    (error "OPENAI_API_KEY environment variable not set"))
  
  (define model (hash-ref config 'model "gpt-4o"))
  
  (define base-request
    (hash 'model model
          'input input
          'temperature (hash-ref config 'temperature 0.7)
          'stream #f))
  
  ;; Add all supported config options
  (define request-body
    (foldl (lambda (key acc)
             (if (hash-has-key? config key)
                 (hash-set acc key (hash-ref config key))
                 acc))
           base-request
           '(max_output_tokens max_tool_calls tools response_format
             parallel_tool_calls previous_response_id instructions
             metadata include)))
  
  (define json-body (jsexpr->string request-body))
  
  (define-values (status headers response-port)
    (http-sendrecv "api.openai.com"
                   "/v1/responses"
                   #:ssl? #t
                   #:method "POST"
                   #:headers (list (format "Authorization: Bearer ~a" api-key)
                                  "Content-Type: application/json")
                   #:data json-body))
  
  (define response-body (port->string response-port))
  
  (unless (bytes=? (subbytes status 9 12) #"200")
    (error (format "OpenAI API error: ~a - ~a" status response-body)))
  
  (define response-json (string->jsexpr response-body))
  
  ;; Extract the text from the response
  (define output (hash-ref response-json 'output '()))
  (if (null? output)
      ""
      (let* ([first-output (car output)]
             [content (hash-ref first-output 'content '())])
        (if (null? content)
            ""
            (let ([first-content (car content)])
              (hash-ref first-content 'text ""))))))