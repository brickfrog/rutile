#lang racket/base

(require racket/match)
(require racket/file)
(require racket/path)
(require "parser.rkt")

(provide module-system-builtin?
         execute-module-builtin
         init-module-system
         resolve-module-path)

;; Module system state
(define loaded-modules (make-hash))
(define module-exports (make-hash))
(define module-search-paths (list "." "modules" "lib"))

(define (init-module-system)
  (set! loaded-modules (make-hash))
  (set! module-exports (make-hash)))

;; Check if a word is a module system builtin
(define (module-system-builtin? name)
  (member name '("import" "export" "module" "from-module" "module-path" "reload-module"
                 "list-modules" "module-exports" "module-info")))

;; Execute module system builtin operations  
(define (execute-module-builtin vm name vm-pop vm-push vm-define-word vm-words make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)
  (match name
    ["import"
     ;; Import module: "module-name" import
     (define module-name (vm-pop vm))
     (load-module vm module-name #f make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)]
    
    ["from-module"
     ;; Import specific words: ["word1" "word2"] "module-name" from-module
     (define module-name (vm-pop vm))
     (define word-list (vm-pop vm))
     (load-module vm module-name word-list make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)]
    
    ["export"
     ;; Export current words: ["word1" "word2"] export
     (define word-list (vm-pop vm))
     (export-words vm word-list)]
    
    ["module"
     ;; Define module: "module-name" { exports: ["word1" "word2"] } module
     (define config (vm-pop vm))
     (define module-name (vm-pop vm))
     (define-module vm module-name config)]
    
    ["module-path"
     ;; Add module search path: "path" module-path
     (define path (vm-pop vm))
     (add-module-path path)]
    
    ["reload-module"
     ;; Reload module: "module-name" reload-module
     (define module-name (vm-pop vm))
     (reload-module vm module-name make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)]
    
    ["list-modules"
     ;; List loaded modules
     (vm-push vm (hash-keys loaded-modules))]
    
    ["module-exports"
     ;; Get module exports: "module-name" module-exports -> [word-list]
     (define module-name (vm-pop vm))
     (define exports (hash-ref module-exports module-name '()))
     (vm-push vm exports)]
    
    ["module-info"
     ;; Get module info: "module-name" module-info -> {info}
     (define module-name (vm-pop vm))
     (define info (get-module-info module-name))
     (vm-push vm info)]
    
    [else (error "Unknown module builtin:" name)]))

;; Resolve module file path
(define (resolve-module-path module-name)
  (define possible-files
    (list (string-append module-name ".rtl")
          (string-append module-name "/init.rtl")
          (string-append module-name "/main.rtl")))
  
  (for/or ([search-path module-search-paths])
    (for/or ([file possible-files])
      (define full-path (build-path search-path file))
      (and (file-exists? full-path) (path->string full-path)))))

;; Load module
(define (load-module vm module-name specific-words make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)
  (cond
    [(hash-has-key? loaded-modules module-name)
     ;; Module already loaded, import words
     (import-module-words vm module-name specific-words vm-lookup-word-fn vm-define-word-fn)]
    [else
     ;; Load module file
     (define module-path (resolve-module-path module-name))
     (if module-path
         (begin
           (load-module-file vm module-name module-path make-vm-fn execute-tokens-fn)
           (import-module-words vm module-name specific-words vm-lookup-word-fn vm-define-word-fn))
         (error "Module not found:" module-name))]))

;; Load module from file
(define (load-module-file vm module-name file-path make-vm-fn execute-tokens-fn)
  (displayln (format "Loading module ~a from ~a" module-name file-path))
  
  ;; Create isolated environment for module
  (define module-vm (make-vm-fn))
  
  ;; Load and execute module file
  (define content (file->string file-path))
  (define tokens (parse-rutile content))
  
  ;; Execute in module context
  (execute-tokens-fn module-vm tokens)
  
  ;; Store module state
  (hash-set! loaded-modules module-name
             (hash 'vm module-vm
                   'path file-path
                   'loaded-at (current-seconds)))
  
  (displayln (format "Module ~a loaded successfully" module-name)))


;; Import words from loaded module
(define (import-module-words vm module-name specific-words vm-lookup-word-fn vm-define-word-fn)
  (define module-info (hash-ref loaded-modules module-name))
  (define module-vm (hash-ref module-info 'vm))
  (define exports (hash-ref module-exports module-name '()))
  
  (define words-to-import
    (if specific-words
        ;; Import only specific words
        (filter (lambda (word) (member word specific-words)) exports)
        ;; Import all exported words
        exports))
  
  (for ([word words-to-import])
    (define word-def (vm-lookup-word-fn module-vm word))
    (when word-def
      (vm-define-word-fn vm word word-def)
      (displayln (format "Imported word: ~a" word)))))

;; Export words from current module
(define (export-words vm word-list)
  ;; For now, just mark words as exported
  ;; In a real implementation, this would be tied to module context
  (displayln (format "Exported words: ~a" word-list)))

;; Define module with configuration
(define (define-module vm module-name config)
  (define exports (hash-ref config 'exports '()))
  (hash-set! module-exports module-name exports)
  (displayln (format "Defined module ~a with exports: ~a" module-name exports)))

;; Add module search path
(define (add-module-path path)
  (set! module-search-paths (cons path module-search-paths))
  (displayln (format "Added module search path: ~a" path)))

;; Reload module
(define (reload-module vm module-name make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)
  (hash-remove! loaded-modules module-name)
  (load-module vm module-name #f make-vm-fn execute-tokens-fn vm-lookup-word-fn vm-define-word-fn)
  (displayln (format "Reloaded module: ~a" module-name)))

;; Get module information
(define (get-module-info module-name)
  (define module-info (hash-ref loaded-modules module-name #f))
  (if module-info
      (hash 'name module-name
            'path (hash-ref module-info 'path)
            'loaded-at (hash-ref module-info 'loaded-at)
            'exports (hash-ref module-exports module-name '()))
      (hash 'error "Module not found")))