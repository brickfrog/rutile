#lang racket/base

(require racket/hash)
(require racket/match)
(require racket/port)
(require racket/system)
(require racket/string)
(require racket/function)
(require json)

(provide claude-builtin?
         execute-claude-builtin
         make-claude-context
         claude-call-simple
         spawn-claude-process
         agent-send-receive)

;; Claude context management
;; Tracks active agents, default config, etc.
(define (make-claude-context)
  (make-hash (list (cons 'agents (make-hash))
                   (cons 'default-model "sonnet")
                   (cons 'default-config (hash)))))

;; Check if a word is a Claude builtin
(define (claude-builtin? name)
  (member name '("claude-call" "claude-stream" "claude-config"
                 "spawn-claude-agent" "agent-send" "agent-receive"
                 "agent-kill" "agent-list" "agent-try-receive"
                 "claude-set-model" "claude-set-config")))

;; Simple synchronous Claude call via CLI
;; Returns just the result text
(define (claude-call-simple prompt #:system-prompt [system-prompt #f]
                                   #:model [model "sonnet"]
                                   #:session-id [session-id #f])
  (define cmd-parts
    (filter identity
      (list "claude" "--print" "--output-format" "json"
            (if system-prompt (format "--system-prompt=~a" system-prompt) #f)
            (if model (format "--model=~a" model) #f)
            (if session-id (format "--session-id=~a" session-id) #f))))

  (define cmd (string-join cmd-parts " "))

  ;; Call Claude and capture output
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f (find-executable-path "claude")
                "--print" "--output-format" "json"
                (if system-prompt (format "--system-prompt=~a" system-prompt) "--system-prompt=")
                (if model (format "--model=~a" model) "--model=sonnet")))

  ;; Send prompt to stdin
  (display prompt stdin)
  (close-output-port stdin)

  ;; Read JSON response
  (define response-json (port->string stdout))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait proc)

  ;; Parse JSON and extract result
  (with-handlers ([exn:fail? (lambda (e)
                               (error (format "Failed to parse Claude response: ~a\nResponse: ~a"
                                            (exn-message e) response-json)))])
    (define response (string->jsexpr response-json))
    (hash-ref response 'result "")))

;; Spawn a named Claude agent process with persistent session
;; Returns agent handle (session-id)
(define (spawn-claude-process name system-prompt)
  (define session-id (string-append "rutile-" name "-" (number->string (current-seconds))))
  (make-hash (list (cons 'name name)
                   (cons 'session-id session-id)
                   (cons 'system-prompt system-prompt)
                   (cons 'created-at (current-seconds)))))

;; Send message to agent and get response
(define (agent-send-receive agent prompt)
  (define session-id (hash-ref agent 'session-id))
  (define system-prompt (hash-ref agent 'system-prompt))
  (claude-call-simple prompt
                      #:system-prompt system-prompt
                      #:session-id session-id))

;; Execute Claude builtin operations
(define (execute-claude-builtin vm name vm-pop vm-push vm-stack vm-claude-context)
  (match name
    ["claude-call"
     ;; Simple Claude call: "prompt" claude-call
     ;; Or with config: {system: "..."} "prompt" claude-call
     (cond
       [(= (length (vm-stack vm)) 1)
        ;; Just prompt - use default config
        (define prompt (vm-pop vm))
        (define model (hash-ref (vm-claude-context vm) 'default-model "sonnet"))
        (define response (claude-call-simple prompt #:model model))
        (vm-push vm response)]
       [else
        ;; Config + prompt
        (define prompt (vm-pop vm))
        (define config (vm-pop vm))
        (define system-prompt (hash-ref config 'system #f))
        (define model (hash-ref config 'model
                               (hash-ref (vm-claude-context vm) 'default-model "sonnet")))
        (define response (claude-call-simple prompt
                                             #:system-prompt system-prompt
                                             #:model model))
        (vm-push vm response)])]

    ["claude-set-model"
     ;; Set default model: "sonnet" claude-set-model
     (define model (vm-pop vm))
     (hash-set! (vm-claude-context vm) 'default-model model)
     (displayln (format "Default Claude model set to: ~a" model))]

    ["claude-set-config"
     ;; Set default config: {system: "You are helpful"} claude-set-config
     (define config (vm-pop vm))
     (hash-set! (vm-claude-context vm) 'default-config config)
     (displayln "Default Claude config updated")]

    ["spawn-claude-agent"
     ;; Spawn agent: "You are a researcher" "researcher" spawn-claude-agent
     (define name (vm-pop vm))
     (define system-prompt (vm-pop vm))
     (define agent (spawn-claude-process name system-prompt))
     (define agents (hash-ref (vm-claude-context vm) 'agents))
     (hash-set! agents name agent)
     (vm-push vm name)
     (displayln (format "Spawned Claude agent: ~a" name))]

    ["agent-send"
     ;; Send to agent: "prompt" agent-name agent-send
     (define agent-name (vm-pop vm))
     (define prompt (vm-pop vm))
     (define agents (hash-ref (vm-claude-context vm) 'agents))
     (define agent (hash-ref agents agent-name
                            (lambda () (error (format "Agent not found: ~a" agent-name)))))
     (define response (agent-send-receive agent prompt))
     (vm-push vm response)]

    ["agent-receive"
     ;; Alias for agent-send for now (will be separated for async later)
     (execute-claude-builtin vm "agent-send" vm-pop vm-push vm-stack vm-claude-context)]

    ["agent-kill"
     ;; Kill agent: agent-name agent-kill
     (define agent-name (vm-pop vm))
     (define agents (hash-ref (vm-claude-context vm) 'agents))
     (hash-remove! agents agent-name)
     (displayln (format "Killed Claude agent: ~a" agent-name))]

    ["agent-list"
     ;; List active agents: agent-list -> [names]
     (define agents (hash-ref (vm-claude-context vm) 'agents))
     (define agent-names (hash-keys agents))
     (vm-push vm agent-names)]

    [else (error "Unknown Claude builtin:" name)]))
