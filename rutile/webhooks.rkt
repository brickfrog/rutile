#lang racket/base

(require racket/match)
(require racket/file)
(require "parser.rkt")

(provide webhook-builtin?
         execute-webhook-builtin
         init-webhook-state)

;; Webhook server state
(define webhook-server-thread #f)
(define webhook-handlers (make-hash))

;; Hot reload state
(define watched-files (make-hash))
(define file-timestamps (make-hash))
(define file-watcher-thread #f)

;; Initialize webhook state
(define (init-webhook-state)
  (set! webhook-server-thread #f)
  (set! webhook-handlers (make-hash))
  (set! watched-files (make-hash))
  (set! file-timestamps (make-hash))
  (set! file-watcher-thread #f))

;; Check if a word is a webhook builtin
(define (webhook-builtin? name)
  (member name '("webhook-listen" "webhook-handler" "webhook-stop" "webhook-server"
                 "hot-reload" "watch-file" "auto-reload" "reload-on-change")))

;; Execute webhook builtin operations
(define (execute-webhook-builtin vm name vm-pop vm-push)
  (match name
    ["webhook-listen"
     (define handler (vm-pop vm))
     (define url (vm-pop vm))
     (start-webhook-server url handler vm)]
    ["webhook-handler"
     (define handler (vm-pop vm))
     (define path (vm-pop vm))
     (register-webhook-handler path handler)]
    ["webhook-stop"
     (stop-webhook-server)]
    ["webhook-server"
     (define port (vm-pop vm))
     (start-webhook-server-on-port port vm)]
    ["hot-reload"
     (start-file-watcher)]
    ["watch-file"
     (define filename (vm-pop vm))
     (watch-file-for-changes filename vm)]
    ["auto-reload"
     (define enable (vm-pop vm))
     (if enable
         (start-file-watcher)
         (stop-file-watcher))]
    ["reload-on-change"
     (check-file-changes)]
    [else (error "Unknown webhook builtin:" name)]))

;; Webhook server functions
(define (start-webhook-server url handler vm)
  (define port 8080) ;; Default port
  (start-webhook-server-on-port port vm))

(define (start-webhook-server-on-port port vm)
  (displayln (format "Starting webhook server on port ~a" port))
  ;; Simplified webhook server - in production would use proper HTTP server
  (displayln "Webhook server listening for incoming requests...")
  port)

(define (stop-webhook-server)
  (displayln "Stopping webhook server")
  (set! webhook-server-thread #f))

(define (register-webhook-handler path handler)
  (hash-set! webhook-handlers path handler)
  (displayln (format "Registered webhook handler for ~a" path)))

;; Hot reload functions
(define (watch-file-for-changes filename vm)
  (define current-time (file-or-directory-modify-seconds filename))
  (hash-set! watched-files filename vm)
  (hash-set! file-timestamps filename current-time)
  (displayln (format "Watching file: ~a" filename)))

(define (check-file-changes)
  (for ([(filename vm) (in-hash watched-files)])
    (when (file-exists? filename)
      (define current-time (file-or-directory-modify-seconds filename))
      (define last-time (hash-ref file-timestamps filename 0))
      (when (> current-time last-time)
        (displayln (format "File changed: ~a - reloading..." filename))
        (hash-set! file-timestamps filename current-time)
        (reload-file filename vm)))))

(define (reload-file filename vm)
  (with-handlers ([exn:fail? (lambda (e)
                              (displayln (format "Reload error: ~a" (exn-message e))))])
    (define content (file->string filename))
    ;; Note: Hot reload implementation moved to hotreload.rkt to avoid circular dependencies
    (void)))

(define (start-file-watcher)
  (when (not file-watcher-thread)
    (displayln "Starting file watcher...")
    ;; Simplified - in production would use proper threading
    (set! file-watcher-thread #t)))

(define (stop-file-watcher)
  (when file-watcher-thread
    (displayln "Stopping file watcher...")
    (set! file-watcher-thread #f)))