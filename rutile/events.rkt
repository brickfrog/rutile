#lang racket

(require racket/match)
(require racket/file)
(require racket/system)
(require racket/string)
(require "parser.rkt")

(provide event-source-builtin?
         execute-event-source-builtin
         init-event-sources
         parse-sigil-syntax
         start-file-watcher
         stop-file-watcher)

;; Event source state
(define timer-threads (make-hash))
(define file-watchers (make-hash))
(define queues (make-hash))

;; Initialize event sources
(define (init-event-sources)
  (set! timer-threads (make-hash))
  (set! file-watchers (make-hash))
  (set! queues (make-hash)))

;; Check if a word starts with @ sigil
(define (sigil-word? word)
  (define word-str (if (symbol? word) (symbol->string word) word))
  (and (string? word-str) 
       (> (string-length word-str) 0)
       (char=? #\@ (string-ref word-str 0))))

;; Parse @sigil.operation syntax
(define (parse-sigil-syntax word)
  (if (sigil-word? word)
      (let* ([word-str (if (symbol? word) (symbol->string word) word)]
             [parts (string-split (substring word-str 1) ".")])
        (if (>= (length parts) 2)
            (values (car parts) (cadr parts))
            (values (car parts) #f)))
      (values #f #f)))

;; Check if a word is an event source builtin
(define (event-source-builtin? name)
  (define name-str (if (symbol? name) (symbol->string name) name))
  (or (sigil-word? name-str)
      (member name-str '("timer-stop" "timer-list" "file-unwatch" "queue-push" 
                        "queue-pop" "queue-create" "queue-destroy" "queue-size"))))

;; Execute event source builtin operations
(define (execute-event-source-builtin vm name vm-pop vm-push execute-tokens-fn)
  (define name-str (if (symbol? name) (symbol->string name) name))
  (cond
    [(sigil-word? name-str)
     (let-values ([(source operation) (parse-sigil-syntax name-str)])
       (execute-sigil-operation vm source operation vm-pop vm-push execute-tokens-fn))]
    [else
     (execute-regular-event-builtin vm name-str vm-pop vm-push)]))

;; Execute @sigil operations
(define (execute-sigil-operation vm source operation vm-pop vm-push execute-tokens-fn)
  (match source
    ["timer"
     (execute-timer-operation vm operation vm-pop vm-push execute-tokens-fn)]
    ["file" 
     (execute-file-operation vm operation vm-pop vm-push execute-tokens-fn)]
    ["queue"
     (execute-queue-operation vm operation vm-pop vm-push)]
    [else 
     (error "Unknown event source:" source)]))

;; Timer operations
(define (execute-timer-operation vm operation vm-pop vm-push execute-tokens-fn)
  (match operation
    ["every"
     ;; Usage: 1000 [code-block] @timer.every -> timer-id
     (define code-block (vm-pop vm))
     (define interval-ms (vm-pop vm))
     (define timer-id (start-timer interval-ms code-block vm execute-tokens-fn))
     (vm-push vm timer-id)]
    ["once"
     ;; Usage: 5000 [code-block] @timer.once -> timer-id  
     (define code-block (vm-pop vm))
     (define delay-ms (vm-pop vm))
     (define timer-id (start-timer-once delay-ms code-block vm execute-tokens-fn))
     (vm-push vm timer-id)]
    [else (error "Unknown timer operation:" operation)]))

;; File operations  
(define (execute-file-operation vm operation vm-pop vm-push execute-tokens-fn)
  (match operation
    ["watch"
     ;; Usage: "filename.txt" [on-change-code] @file.watch -> watcher-id
     (define code-block (vm-pop vm))
     (define filename (vm-pop vm))
     (define watcher-id (start-file-watcher filename code-block vm execute-tokens-fn))
     (vm-push vm watcher-id)]
    ["poll"
     ;; Usage: "filename.txt" 1000 [on-change-code] @file.poll -> watcher-id
     (define code-block (vm-pop vm))
     (define interval-ms (vm-pop vm))
     (define filename (vm-pop vm))
     (define watcher-id (start-file-poller filename interval-ms code-block vm execute-tokens-fn))
     (vm-push vm watcher-id)]
    [else (error "Unknown file operation:" operation)]))

;; Queue operations
(define (execute-queue-operation vm operation vm-pop vm-push)
  (match operation
    ["poll"
     ;; Usage: "queue-name" 1000 [on-message-code] @queue.poll -> poller-id
     (define code-block (vm-pop vm))
     (define interval-ms (vm-pop vm))
     (define queue-name (vm-pop vm))
     (define poller-id (start-queue-poller queue-name interval-ms code-block vm))
     (vm-push vm poller-id)]
    ["create"
     ;; Usage: "queue-name" @queue.create
     (define queue-name (vm-pop vm))
     (create-queue queue-name)
     (vm-push vm queue-name)]
    [else (error "Unknown queue operation:" operation)]))

;; Regular event builtin operations
(define (execute-regular-event-builtin vm name vm-pop vm-push)
  (match name
    ["timer-stop"
     (define timer-id (vm-pop vm))
     (stop-timer timer-id)]
    ["timer-list"
     (vm-push vm (hash-keys timer-threads))]
    ["file-unwatch"
     (define watcher-id (vm-pop vm))
     (stop-file-watcher watcher-id)]
    ["queue-push"
     ;; Usage: value "queue-name" queue-push
     (define queue-name (vm-pop vm))
     (define value (vm-pop vm))
     (queue-push queue-name value)]
    ["queue-pop"
     ;; Usage: "queue-name" queue-pop -> value
     (define queue-name (vm-pop vm))
     (define value (queue-pop queue-name))
     (vm-push vm value)]
    ["queue-size"
     (define queue-name (vm-pop vm))
     (define size (queue-size queue-name))
     (vm-push vm size)]
    ["queue-destroy"
     (define queue-name (vm-pop vm))
     (destroy-queue queue-name)]
    [else (error "Unknown event builtin:" name)]))

;; Timer implementation with proper threading
(define (start-timer interval-ms code-block vm execute-tokens-fn)
  (define timer-id (symbol->string (gensym 'timer)))
  (define timer-thread
    (thread 
     (lambda ()
       (let loop ()
         (sleep (/ interval-ms 1000.0))
         (with-handlers ([exn:fail? (lambda (e)
                                     (displayln (format "Timer error: ~a" (exn-message e))))])
           (execute-tokens-fn vm code-block))
         (when (hash-has-key? timer-threads timer-id)
           (loop))))))
  (hash-set! timer-threads timer-id timer-thread)
  (displayln (format "Started timer ~a with interval ~ams" timer-id interval-ms))
  timer-id)

(define (start-timer-once delay-ms code-block vm execute-tokens-fn)
  (define timer-id (symbol->string (gensym 'timer-once)))
  (define timer-thread
    (thread
     (lambda ()
       (sleep (/ delay-ms 1000.0))
       (with-handlers ([exn:fail? (lambda (e)
                                   (displayln (format "Timer error: ~a" (exn-message e))))])
         (execute-tokens-fn vm code-block))
       (hash-remove! timer-threads timer-id))))
  (hash-set! timer-threads timer-id timer-thread)
  (displayln (format "Started one-shot timer ~a with delay ~ams" timer-id delay-ms))
  timer-id)

(define (stop-timer timer-id)
  (define timer-thread (hash-ref timer-threads timer-id #f))
  (when timer-thread
    (kill-thread timer-thread)
    (hash-remove! timer-threads timer-id)
    (displayln (format "Stopped timer ~a" timer-id))))

;; File watcher implementation with proper threading
(define (start-file-watcher filename code-block vm execute-tokens-fn)
  (define watcher-id (symbol->string (gensym 'watcher)))
  (if (file-exists? filename)
      (let ([last-modified (file-or-directory-modify-seconds filename)])
        (define watcher-thread
          (thread
           (lambda ()
             (let loop ([last-time last-modified])
               (sleep 0.5) ;; Check every 500ms
               (when (and (file-exists? filename)
                         (hash-has-key? file-watchers watcher-id))
                 (define current-time (file-or-directory-modify-seconds filename))
                 (if (> current-time last-time)
                     (begin
                       (displayln (format "File changed: ~a" filename))
                       (with-handlers ([exn:fail? (lambda (e)
                                                   (displayln (format "File watcher error: ~a" (exn-message e))))])
                         (execute-tokens-fn vm code-block))
                       (loop current-time))
                     (loop last-time)))))))
        (hash-set! file-watchers watcher-id (list watcher-thread filename))
        (displayln (format "Started watching file: ~a" filename))
        watcher-id)
      (error "File does not exist:" filename)))

(define (start-file-poller filename interval-ms code-block vm execute-tokens-fn)
  (define watcher-id (symbol->string (gensym 'file-poller)))
  (if (file-exists? filename)
      (let ([last-modified (file-or-directory-modify-seconds filename)])
        (define poller-thread
          (thread
           (lambda ()
             (let loop ([last-time last-modified])
               (sleep (/ interval-ms 1000.0))
               (when (and (file-exists? filename)
                         (hash-has-key? file-watchers watcher-id))
                 (define current-time (file-or-directory-modify-seconds filename))
                 (if (> current-time last-time)
                     (begin
                       (displayln (format "File changed: ~a" filename))
                       (with-handlers ([exn:fail? (lambda (e)
                                                   (displayln (format "File poller error: ~a" (exn-message e))))])
                         (execute-tokens-fn vm code-block))
                       (loop current-time))
                     (loop last-time)))))))
        (hash-set! file-watchers watcher-id (list poller-thread filename))
        (displayln (format "Started polling file: ~a every ~ams" filename interval-ms))
        watcher-id)
      (error "File does not exist:" filename)))

(define (stop-file-watcher watcher-id)
  (define watcher-info (hash-ref file-watchers watcher-id #f))
  (when watcher-info
    (define watcher-thread (car watcher-info))
    (define filename (cadr watcher-info))
    (kill-thread watcher-thread)
    (hash-remove! file-watchers watcher-id)
    (displayln (format "Stopped watching file: ~a" filename))))

;; Queue implementation
(define (create-queue queue-name)
  (hash-set! queues queue-name '())
  (displayln (format "Created queue: ~a" queue-name)))

(define (queue-push queue-name value)
  (define current-queue (hash-ref queues queue-name '()))
  (hash-set! queues queue-name (append current-queue (list value)))
  (displayln (format "Pushed to queue ~a: ~a" queue-name value)))

(define (queue-pop queue-name)
  (define current-queue (hash-ref queues queue-name '()))
  (if (null? current-queue)
      #f
      (let ([value (car current-queue)])
        (hash-set! queues queue-name (cdr current-queue))
        (displayln (format "Popped from queue ~a: ~a" queue-name value))
        value)))

(define (queue-size queue-name)
  (length (hash-ref queues queue-name '())))

(define (destroy-queue queue-name)
  (hash-remove! queues queue-name)
  (displayln (format "Destroyed queue: ~a" queue-name)))

(define (start-queue-poller queue-name interval-ms code-block vm)
  (define poller-id (symbol->string (gensym 'queue-poller)))
  (define poller-thread
    (thread
     (lambda ()
       (let loop ()
         (sleep (/ interval-ms 1000.0))
         (when (hash-has-key? queues queue-name)
           (define message (queue-pop queue-name))
           (when message
             (with-handlers ([exn:fail? (lambda (e)
                                        (displayln (format "Queue poller error: ~a" (exn-message e))))])
               ;; Note: Message processing implementation depends on VM context
               (when (not (string=? message ""))
                 (displayln (format "Queue poller processing: ~a" message)))))
           (when (hash-has-key? timer-threads poller-id)
             (loop)))))))
  (hash-set! timer-threads poller-id poller-thread)
  (displayln (format "Started queue poller for ~a every ~ams" queue-name interval-ms))
  poller-id)