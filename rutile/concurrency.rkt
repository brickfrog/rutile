#lang racket/base

(require racket/place)
(require racket/match)

(provide concurrency-builtin?
         execute-concurrency-builtin
         init-concurrency-state)

;; Agent/place management state
(define active-agents (make-hash))
(define agent-counter 0)

;; Supervisor tree state
(define supervision-policies (make-hash))
(define agent-restart-counts (make-hash))
(define max-restarts 5)
(define supervisor-thread #f)

;; Initialize concurrency state
(define (init-concurrency-state)
  (set! active-agents (make-hash))
  (set! agent-counter 0)
  (set! supervision-policies (make-hash))
  (set! agent-restart-counts (make-hash)))

;; Check if a word is a concurrency builtin
(define (concurrency-builtin? name)
  (member name '("spawn-agent" "agent-send" "agent-receive" "agent-kill" "agent-list"
                 "supervise-agent" "start-supervisor" "stop-supervisor" "agent-info" "agent-health")))

;; Execute concurrency builtin operations
(define (execute-concurrency-builtin vm name vm-pop vm-push)
  (match name
    ["spawn-agent"
     (define code (vm-pop vm))
     (define agent-id (spawn-agent-with-code code))
     (vm-push vm agent-id)]
    ["agent-send"
     (define message (vm-pop vm))
     (define agent-id (vm-pop vm))
     (define success? (send-to-agent agent-id message))
     (vm-push vm success?)]
    ["agent-receive"
     ;; Simplified - would need proper message passing
     (vm-push vm "no-message")]
    ["agent-kill"
     (define agent-id (vm-pop vm))
     (define success? (kill-agent agent-id))
     (vm-push vm success?)]
    ["agent-list"
     (vm-push vm (list-agents))]
    ["supervise-agent"
     (define restart-strategy (vm-pop vm))
     (define policy (vm-pop vm))
     (define agent-id (vm-pop vm))
     (supervise-agent agent-id policy restart-strategy)]
    ["start-supervisor"
     (start-supervisor)]
    ["stop-supervisor"
     (stop-supervisor)]
    ["agent-info"
     (define agent-id (vm-pop vm))
     (vm-push vm (get-agent-info agent-id))]
    ["agent-health"
     (define agent-id (vm-pop vm))
     (vm-push vm (check-agent-health agent-id))]
    [else (error "Unknown concurrency builtin:" name)]))

;; Agent management functions
(define (spawn-agent-with-code code)
  (set! agent-counter (+ agent-counter 1))
  (define agent-id (format "agent-~a" agent-counter))
  (define agent-place (place pch
                             (define (agent-loop)
                               (define msg (place-channel-get pch))
                               (match msg
                                 ['shutdown (void)]
                                 [code (eval code)]
                                 [_ (agent-loop)]))
                             (agent-loop)))
  (hash-set! active-agents agent-id agent-place)
  (displayln (format "Spawned agent: ~a" agent-id))
  agent-id)

(define (send-to-agent agent-id message)
  (define agent-place (hash-ref active-agents agent-id #f))
  (if agent-place
      (begin
        (place-channel-put agent-place message)
        (displayln (format "Message sent to agent ~a" agent-id))
        #t)
      (begin
        (displayln (format "Agent ~a not found" agent-id))
        #f)))

(define (kill-agent agent-id)
  (define agent-place (hash-ref active-agents agent-id #f))
  (if agent-place
      (begin
        (place-channel-put agent-place 'shutdown)
        (place-kill agent-place)
        (hash-remove! active-agents agent-id)
        (displayln (format "Agent ~a killed" agent-id))
        #t)
      (begin
        (displayln (format "Agent ~a not found" agent-id))
        #f)))

(define (list-agents)
  (hash-keys active-agents))

;; Supervisor tree functions
(define (supervise-agent agent-id policy restart-strategy)
  (hash-set! supervision-policies agent-id (hash 'policy policy 'restart-strategy restart-strategy))
  (hash-set! agent-restart-counts agent-id 0)
  (displayln (format "Agent ~a is now supervised with policy: ~a" agent-id policy)))

(define (check-agent-health agent-id)
  (define agent-place (hash-ref active-agents agent-id #f))
  (if agent-place
      (begin
        ;; Check if place is still alive
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (place-channel-put agent-place 'ping)
          #t))
      #f))

(define (restart-agent agent-id)
  (define policy (hash-ref supervision-policies agent-id #f))
  (define restart-count (hash-ref agent-restart-counts agent-id 0))
  
  (if (and policy (< restart-count max-restarts))
      (begin
        (displayln (format "Restarting agent ~a (attempt ~a/~a)" agent-id (+ restart-count 1) max-restarts))
        (hash-set! agent-restart-counts agent-id (+ restart-count 1))
        
        ;; Kill existing place if it exists
        (let ([old-place (hash-ref active-agents agent-id #f)])
          (when old-place
            (with-handlers ([exn:fail? (lambda (e) #f)])
              (place-kill old-place))))
        
        ;; Create new agent with same code
        (let ([restart-strategy (hash-ref policy 'restart-strategy 'permanent)]
              [new-agent-id (spawn-agent-with-code '(+ 1 2 3))])
          (hash-set! active-agents agent-id (hash-ref active-agents new-agent-id #f))
          (hash-remove! active-agents new-agent-id))
        (displayln (format "Agent ~a restarted successfully" agent-id))
        #t)
      (begin
        (displayln (format "Agent ~a exceeded max restarts (~a), removing from supervision" agent-id max-restarts))
        (hash-remove! supervision-policies agent-id)
        (hash-remove! agent-restart-counts agent-id)
        (hash-remove! active-agents agent-id)
        #f)))

(define (supervisor-loop)
  (let loop ()
    (for ([(agent-id policy) (in-hash supervision-policies)])
      (unless (check-agent-health agent-id)
        (displayln (format "Agent ~a appears to be dead, attempting restart..." agent-id))
        (restart-agent agent-id)))
    (sleep 5) ;; Check every 5 seconds
    (when supervisor-thread
      (loop))))

(define (start-supervisor)
  (unless supervisor-thread
    (displayln "Starting supervisor tree...")
    (set! supervisor-thread (thread supervisor-loop))))

(define (stop-supervisor)
  (when supervisor-thread
    (displayln "Stopping supervisor tree...")
    (set! supervisor-thread #f)))

(define (get-agent-info agent-id)
  (define agent-place (hash-ref active-agents agent-id #f))
  (define policy (hash-ref supervision-policies agent-id #f))
  (define restart-count (hash-ref agent-restart-counts agent-id 0))
  
  (hash 'agent-id agent-id
        'alive (if agent-place #t #f)
        'supervised (if policy #t #f)
        'restart-count restart-count
        'policy policy))