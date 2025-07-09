#lang racket

(require racket/match)
(require racket/string)
(require "vm.rkt")
(require "parser.rkt")

(provide debug-repl
         vm-debugger
         set-breakpoint
         remove-breakpoint
         step-over
         step-into
         continue-execution
         inspect-stack
         inspect-words
         trace-execution
         debug-repl-with-features)

;; Debugger state
(struct debugger-state (vm breakpoints step-mode trace-enabled call-stack) #:mutable)

;; Create debugger
(define (vm-debugger vm)
  (debugger-state vm '() 'none #f '()))

;; Debug REPL
(define (debug-repl debugger)
  (displayln "Rutile Debugger v0.1.0")
  (displayln "Type 'help' for commands, 'quit' to exit")
  (debug-repl-loop debugger))

(define (debug-repl-loop debugger)
  (display "debug> ")
  (flush-output)
  (define input (read-line))
  (cond
    [(eof-object? input) (void)]
    [(string=? input "quit") (void)]
    [(string=? input "help") 
     (show-debug-help)
     (debug-repl-loop debugger)]
    [(string=? input "stack")
     (inspect-stack (debugger-state-vm debugger))
     (debug-repl-loop debugger)]
    [(string=? input "words")
     (inspect-words (debugger-state-vm debugger))
     (debug-repl-loop debugger)]
    [(string=? input "step")
     (step-over debugger)
     (debug-repl-loop debugger)]
    [(string=? input "stepi")
     (step-into debugger)  
     (debug-repl-loop debugger)]
    [(string=? input "continue")
     (continue-execution debugger)
     (debug-repl-loop debugger)]
    [(string=? input "trace on")
     (set-debugger-state-trace-enabled! debugger #t)
     (displayln "Tracing enabled")
     (debug-repl-loop debugger)]
    [(string=? input "trace off")
     (set-debugger-state-trace-enabled! debugger #f) 
     (displayln "Tracing disabled")
     (debug-repl-loop debugger)]
    [(string-prefix? input "break ")
     (define word (substring input 6))
     (set-breakpoint debugger word)
     (debug-repl-loop debugger)]
    [(string-prefix? input "unbreak ")
     (define word (substring input 8))
     (remove-breakpoint debugger word)
     (debug-repl-loop debugger)]
    [(string=? input "breakpoints")
     (show-breakpoints debugger)
     (debug-repl-loop debugger)]
    [else
     ;; Execute rutile code
     (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (format "Error: ~a" (exn-message e))))])
       (define tokens (parse-rutile input))
       (execute-tokens-debug (debugger-state-vm debugger) tokens debugger))
     (debug-repl-loop debugger)]))

(define (show-debug-help)
  (displayln "Debug commands:")
  (displayln "  help           - Show this help")
  (displayln "  quit           - Exit debugger")
  (displayln "  stack          - Show current stack")
  (displayln "  words          - Show defined words")
  (displayln "  step           - Step over next instruction")
  (displayln "  stepi          - Step into next instruction")
  (displayln "  continue       - Continue execution")
  (displayln "  break <word>   - Set breakpoint on word")
  (displayln "  unbreak <word> - Remove breakpoint")
  (displayln "  breakpoints    - Show all breakpoints")
  (displayln "  trace on/off   - Enable/disable execution tracing")
  (displayln "  <rutile code>  - Execute rutile code"))

;; Breakpoint management
(define (set-breakpoint debugger word)
  (define breakpoints (debugger-state-breakpoints debugger))
  (unless (member word breakpoints)
    (set-debugger-state-breakpoints! debugger (cons word breakpoints))
    (displayln (format "Breakpoint set on: ~a" word))))

(define (remove-breakpoint debugger word)
  (define breakpoints (debugger-state-breakpoints debugger))
  (set-debugger-state-breakpoints! debugger (remove word breakpoints))
  (displayln (format "Breakpoint removed from: ~a" word)))

(define (show-breakpoints debugger)
  (define breakpoints (debugger-state-breakpoints debugger))
  (if (null? breakpoints)
      (displayln "No breakpoints set")
      (begin
        (displayln "Breakpoints:")
        (for ([bp breakpoints])
          (displayln (format "  ~a" bp))))))

;; Stepping controls
(define (step-over debugger)
  (set-debugger-state-step-mode! debugger 'over)
  (displayln "Step over mode enabled"))

(define (step-into debugger)
  (set-debugger-state-step-mode! debugger 'into)
  (displayln "Step into mode enabled"))

(define (continue-execution debugger)
  (set-debugger-state-step-mode! debugger 'none)
  (displayln "Continuing execution"))

;; Inspection functions
(define (inspect-stack vm)
  (define stack (vm-stack vm))
  (displayln (format "Stack depth: ~a" (length stack)))
  (if (null? stack)
      (displayln "Stack is empty")
      (begin
        (displayln "Stack contents (top to bottom):")
        (for ([i (in-naturals)]
              [val stack])
          (displayln (format "  [~a] ~a" i val))))))

(define (inspect-words vm)
  (define words (hash-keys (vm-words vm)))
  (displayln (format "Defined words (~a):" (length words)))
  (for ([word (sort words string<?)])
    (displayln (format "  ~a" word))))

;; Execution tracing
(define (trace-execution debugger operation details)
  (when (debugger-state-trace-enabled debugger)
    (displayln (format "TRACE: ~a ~a" operation details))))

;; Debug-aware token execution
(define (execute-tokens-debug vm tokens debugger)
  (for ([token tokens])
    (execute-token-debug vm token debugger)))

(define (execute-token-debug vm token debugger)
  (trace-execution debugger "EXEC" token)
  
  ;; Check for breakpoints
  (when (and (symbol? token)
             (member (symbol->string token) (debugger-state-breakpoints debugger)))
    (displayln (format "Breakpoint hit: ~a" token))
    (set-debugger-state-step-mode! debugger 'into))
  
  ;; Handle stepping
  (when (not (eq? (debugger-state-step-mode debugger) 'none))
    (displayln (format "Stepping: ~a" token))
    (displayln "Press Enter to continue, or enter debug command:")
    (define input (read-line))
    (unless (string=? input "")
      ;; Process debug command
      (void)))
  
  ;; Execute the token
  (execute-tokens vm (list token)))

;; Performance profiler
(struct profile-entry (name call-count total-time) #:mutable)

(define profiler-data (make-hash))

(define (start-profiling)
  (set! profiler-data (make-hash))
  (displayln "Profiling started"))

(define (stop-profiling)
  (displayln "Profiling stopped")
  (show-profile-results))

(define (profile-call name thunk)
  (define start-time (current-inexact-milliseconds))
  (define result (thunk))
  (define end-time (current-inexact-milliseconds))
  (define elapsed (- end-time start-time))
  
  (define entry (hash-ref profiler-data name 
                         (lambda () (profile-entry name 0 0.0))))
  (set-profile-entry-call-count! entry (+ (profile-entry-call-count entry) 1))
  (set-profile-entry-total-time! entry (+ (profile-entry-total-time entry) elapsed))
  (hash-set! profiler-data name entry)
  
  result)

(define (show-profile-results)
  (displayln "Profile Results:")
  (displayln "Name                    Calls    Total Time (ms)  Avg Time (ms)")
  (displayln "--------------------------------------------------------")
  
  (define entries (sort (hash-values profiler-data)
                       (lambda (a b) 
                         (> (profile-entry-total-time a)
                            (profile-entry-total-time b)))))
  
  (for ([entry entries])
    (define name (profile-entry-name entry))
    (define calls (profile-entry-call-count entry))
    (define total (profile-entry-total-time entry))
    (define avg (if (> calls 0) (/ total calls) 0.0))
    
    (displayln (format "~a~a~a~a~a"
                      (~a name #:width 20 #:align 'left)
                      "    "
                      (~a calls #:width 8 #:align 'right)
                      "    "
                      (~a (~r total #:precision 2) #:width 15 #:align 'right)
                      "    "
                      (~a (~r avg #:precision 2) #:width 12 #:align 'right)))))

;; Development REPL with debugging features
(define (debug-repl-with-features vm)
  (define debugger (vm-debugger vm))
  (displayln "Rutile Development REPL v0.1.0")
  (displayln "Type ':help' for help, ':debug' for debugger, ':quit' to exit")
  (dev-repl-loop vm debugger))

(define (dev-repl-loop vm debugger)
  (display "dev> ")
  (flush-output)
  (define input (read-line))
  (cond
    [(eof-object? input) (void)]
    [(string=? input ":quit") (void)]
    [(string=? input ":debug")
     (debug-repl debugger)
     (dev-repl-loop vm debugger)]
    [(string=? input ":profile start")
     (start-profiling)
     (dev-repl-loop vm debugger)]
    [(string=? input ":profile stop")
     (stop-profiling)
     (dev-repl-loop vm debugger)]
    [(string=? input ":help")
     (show-dev-help)
     (dev-repl-loop vm debugger)]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (format "Error: ~a" (exn-message e))))])
       (define tokens (parse-rutile input))
       (execute-tokens vm tokens))
     (dev-repl-loop vm debugger)]))

(define (show-dev-help)
  (displayln "Development REPL commands:")
  (displayln "  :help           - Show this help")
  (displayln "  :quit           - Exit REPL")
  (displayln "  :debug          - Enter debugger")
  (displayln "  :profile start  - Start performance profiling")
  (displayln "  :profile stop   - Stop profiling and show results")
  (displayln "  <rutile code>   - Execute rutile code"))