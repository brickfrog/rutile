#lang racket

(require "parser.rkt")
(require "events.rkt")
(require racket/file)
(require racket/string)

(provide hot-reload-builtin?
         execute-hot-reload-builtin
         init-hot-reload)

;; Hot reload state
(define watched-sources (make-hash))
(define reload-watchers (make-hash))

;; Initialize hot reload system
(define (init-hot-reload)
  (set! watched-sources (make-hash))
  (set! reload-watchers (make-hash)))

;; Check if a word is a hot reload builtin
(define (hot-reload-builtin? name)
  (define name-str (if (symbol? name) (symbol->string name) name))
  (member name-str '("hot-reload-enable" "hot-reload-disable" "hot-reload-watch" 
                     "hot-reload-unwatch" "hot-reload-status" "hot-reload-manual")))

;; Execute hot reload builtin operations
(define (execute-hot-reload-builtin vm name vm-pop vm-push execute-tokens-fn vm-words vm-define-word)
  (define name-str (if (symbol? name) (symbol->string name) name))
  (match name-str
    ["hot-reload-enable"
     ;; Enable hot reload for current script: "filename.rtl" hot-reload-enable
     (define filename (vm-pop vm))
     (enable-hot-reload vm filename execute-tokens-fn vm-words vm-define-word)]
    
    ["hot-reload-disable"
     ;; Disable hot reload: "filename.rtl" hot-reload-disable
     (define filename (vm-pop vm))
     (disable-hot-reload filename)]
    
    ["hot-reload-watch"
     ;; Watch a specific file for hot reload: "filename.rtl" hot-reload-watch
     (define filename (vm-pop vm))
     (watch-for-hot-reload vm filename execute-tokens-fn vm-words vm-define-word)]
    
    ["hot-reload-unwatch"
     ;; Stop watching a file: "filename.rtl" hot-reload-unwatch
     (define filename (vm-pop vm))
     (unwatch-hot-reload filename)]
    
    ["hot-reload-status"
     ;; Get list of watched files: hot-reload-status -> [files]
     (vm-push vm (hash-keys watched-sources))]
    
    ["hot-reload-manual"
     ;; Manually reload a file: "filename.rtl" hot-reload-manual
     (define filename (vm-pop vm))
     (manual-reload vm filename execute-tokens-fn vm-words vm-define-word)]
    
    [else (error "Unknown hot reload builtin:" name-str)]))

;; Enable hot reload for a file
(define (enable-hot-reload vm filename execute-tokens-fn vm-words vm-define-word)
  (if (file-exists? filename)
      (let* ([reload-handler
              (lambda ()
                (reload-file vm filename execute-tokens-fn vm-words vm-define-word))]
             [watcher-id (start-file-watcher filename (list reload-handler) vm execute-tokens-fn)])
        ;; Store initial state
        (hash-set! watched-sources filename (hash-copy (vm-words vm)))
        ;; Store watcher ID
        (hash-set! reload-watchers filename watcher-id)
        (displayln (format "Hot reload enabled for: ~a" filename)))
      (error "File does not exist:" filename)))

;; Disable hot reload for a file
(define (disable-hot-reload filename)
  (define watcher-id (hash-ref reload-watchers filename #f))
  (when watcher-id
    (stop-file-watcher watcher-id)
    (hash-remove! reload-watchers filename)
    (hash-remove! watched-sources filename)
    (displayln (format "Hot reload disabled for: ~a" filename))))

;; Watch a file for hot reload (alternative interface)
(define (watch-for-hot-reload vm filename execute-tokens-fn vm-words vm-define-word)
  (enable-hot-reload vm filename execute-tokens-fn vm-words vm-define-word))

;; Stop watching a file
(define (unwatch-hot-reload filename)
  (disable-hot-reload filename))

;; Manually reload a file
(define (manual-reload vm filename execute-tokens-fn vm-words vm-define-word)
  (if (file-exists? filename)
      (reload-file vm filename execute-tokens-fn vm-words vm-define-word)
      (error "File does not exist:" filename)))

;; Core file reload logic
(define (reload-file vm filename execute-tokens-fn vm-words vm-define-word)
  (displayln (format "Hot reloading: ~a" filename))
  (with-handlers ([exn:fail? (lambda (e)
                              (displayln (format "Hot reload error in ~a: ~a" filename (exn-message e))))])
    ;; Get current word definitions for comparison
    (define old-words (hash-ref watched-sources filename (make-hash)))
    
    ;; Load and parse the file
    (define content (file->string filename))
    (define tokens (parse-rutile content))
    
    ;; Execute in current VM context
    (execute-tokens-fn vm tokens)
    
    ;; Update stored state
    (hash-set! watched-sources filename (hash-copy (vm-words vm)))
    
    ;; Report what changed
    (report-changes old-words (vm-words vm))
    
    (displayln (format "Hot reload complete: ~a" filename))))

;; Report what definitions changed during reload
(define (report-changes old-words new-words)
  (define old-keys (hash-keys old-words))
  (define new-keys (hash-keys new-words))
  
  ;; New definitions
  (define added (filter (lambda (key) (not (hash-has-key? old-words key))) new-keys))
  (when (not (null? added))
    (displayln (format "  Added words: ~a" added)))
  
  ;; Removed definitions  
  (define removed (filter (lambda (key) (not (hash-has-key? new-words key))) old-keys))
  (when (not (null? removed))
    (displayln (format "  Removed words: ~a" removed)))
  
  ;; Modified definitions
  (define common-keys (filter (lambda (key) (hash-has-key? new-words key)) old-keys))
  (define modified (filter (lambda (key) 
                            (not (equal? (hash-ref old-words key)
                                        (hash-ref new-words key)))) common-keys))
  (when (not (null? modified))
    (displayln (format "  Modified words: ~a" modified))))

;; Convenience function for REPL integration
(define (enable-repl-hot-reload vm current-file execute-tokens-fn vm-words vm-define-word)
  (when current-file
    (enable-hot-reload vm current-file execute-tokens-fn vm-words vm-define-word)
    (displayln "Hot reload enabled for current session")))