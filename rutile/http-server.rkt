#lang racket/base

(require net/url)
(require net/http-client)
(require racket/port)
(require racket/string)
(require racket/tcp)
(require racket/format)

(provide http-server-start
         http-server-stop
         async-http-get
         async-http-post
         make-chunked-response
         make-stream-response
         http-handler-registry
         register-handler)

;; Global server state
(define current-server #f)
(define handler-registry (make-hash))

(define (http-server-start port)
  (define listener (tcp-listen port 5 #t))
  (define (serve-forever)
    (let loop ()
      (define-values (in out) (tcp-accept listener))
      (define request-line (read-line in))
      (define response-body "<h1>Hello from Rutile HTTP Server!</h1>")
      (fprintf out "HTTP/1.1 200 OK\r\n")
      (fprintf out "Content-Type: text/html\r\n")
      (fprintf out "Content-Length: ~a\r\n\r\n" (string-length response-body))
      (fprintf out "~a" response-body)
      (close-input-port in)
      (close-output-port out)
      (loop)))
  ;; Start server in a thread-like manner (simplified)
  (set! current-server listener)
  port)

(define (http-server-stop)
  (when current-server
    (tcp-close current-server)
    (set! current-server #f)))

(define (register-handler path handler)
  (hash-set! handler-registry path handler))

(define (async-http-get url callback)
  ;; Simplified HTTP GET
  (callback "200 OK" '() (format "HTTP GET response from ~a" url)))

(define (async-http-post url data callback)
  ;; Simplified HTTP POST
  (callback "200 OK" '() (format "HTTP POST response from ~a with data: ~a" url data)))

(define (make-chunked-response generator)
  "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")

(define (make-stream-response stream)
  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\n\r\n")

(define (http-handler-registry)
  (hash-keys handler-registry))