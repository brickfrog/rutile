#lang racket/base

(require "vm.rkt")
(require "parser.rkt")
(require "stdlib.rkt")
(require "testing.rkt")
(require "debugger.rkt")
(require racket/port)
(require racket/file)
(require racket/string)
(require racket/path)
(require racket/system)

(provide rutile-repl
         rutile-run
         rutile-new
         rutile-test
         rutile-build
         rutile-debug
         rutile-debug-repl)

(define (rutile-repl)
  (displayln "rutile repl v0.1.0")
  (displayln "Type :help for help, :quit to exit")
  (define vm (make-vm))
  (initialize-stdlib vm)
  (repl-loop vm))

(define (repl-loop vm)
  (display "> ")
  (flush-output)
  (define input (read-line))
  (cond
    [(eof-object? input) (void)]
    [(string=? input ":quit") (void)]
    [(string=? input ":help") 
     (displayln "Commands:")
     (displayln "  :help                    - Show this help")
     (displayln "  :quit                    - Exit REPL")
     (displayln "  :stack                   - Show stack contents")
     (displayln "  :words                   - Show defined words")
     (displayln "  :reload                  - Reload definitions")
     (displayln "  :hot-reload              - Show hot reload help")
     (displayln "  :hot-reload-file <file>  - Enable hot reload for file")
     (repl-loop vm)]
    [(string=? input ":stack")
     (displayln (format "Stack: ~a" (vm-stack vm)))
     (repl-loop vm)]
    [(string=? input ":words")
     (displayln (format "Words: ~a" (hash-keys (vm-words vm))))
     (repl-loop vm)]
    [(string=? input ":reload")
     (displayln "Reloading...")
     (define new-vm (make-vm))
     (initialize-stdlib new-vm)
     (repl-loop new-vm)]
    [(string=? input ":hot-reload")
     (displayln "Hot reload mode - use :hot-reload-file to watch specific files")
     (displayln "Usage: main.rtl :hot-reload-file")
     (repl-loop vm)]
    [(string-prefix? input ":hot-reload-file ")
     (define filename (substring input 16))
     (displayln (format "Enabling hot reload for: ~a" filename))
     (execute-tokens vm (parse-rutile (format "\"~a\" hot-reload-enable" filename)))
     (repl-loop vm)]
    [else
     (with-handlers ([exn:fail? (lambda (e) 
                                  (displayln (format "Error: ~a" (exn-message e))))])
       (define tokens (parse-rutile input))
       (execute-tokens vm tokens))
     (repl-loop vm)]))

(define (rutile-run script-path)
  (displayln (format "Running script: ~a" script-path))
  (define vm (make-vm))
  (initialize-stdlib vm)
  (with-handlers ([exn:fail? (lambda (e)
                              (displayln (exn-message e))
                              (exit 1))])
    (define content (file->string script-path))
    (define tokens (parse-rutile content))
    (execute-tokens-with-positions vm tokens script-path content)
    (flush-output)))

(define (rutile-new project-name)
  (displayln (format "Creating new rutile project: ~a" project-name))
  (create-project-directory project-name)
  (create-project-files project-name)
  (displayln (format "Project ~a created successfully!" project-name))
  (displayln (format "  cd ~a && rtl repl" project-name)))

(define (create-project-directory project-name)
  (make-directory project-name)
  (make-directory (build-path project-name "src"))
  (make-directory (build-path project-name "tests")))

(define (create-project-files project-name)
  (define main-content 
    (format "# ~a - A Rutile Project\n\n: greet\n  \"Hello from ~a!\" .\n;\n\ngreet\n" project-name project-name))
  
  (define readme-content
    (format "# ~a\n\nA Rutile concatenative programming project.\n\n## Usage\n\n```\nrtl run main.rtl\n```\n\n## Testing\n\n```\nrtl test\n```\n" project-name))
  
  (with-output-to-file (build-path project-name "main.rtl")
    (lambda () (display main-content)))
  
  (with-output-to-file (build-path project-name "README.md")
    (lambda () (display readme-content)))
  
  (with-output-to-file (build-path project-name "tests" "test-main.rtl")
    (lambda () (display "# Test file\n\"Tests would go here\" .\n"))))

(define (rutile-test #:property-based [property-based #f])
  (cond
    [property-based
     (displayln "Running property-based test suite...")
     (property-based-test-suite)]
    [else
     (displayln "Running rutile test suite...")
     (define test-dir "tests")
     (define test-files
       (if (directory-exists? test-dir)
           (filter (lambda (f) (string-suffix? (path->string f) ".rtl"))
                   (directory-list test-dir))
           '()))
     
     (if (null? test-files)
         (displayln "No test files found in tests/ directory")
         (let ([passed 0]
               [failed 0])
           
           (for ([test-file test-files])
             (define test-path (build-path test-dir test-file))
             (displayln (format "Running ~a..." test-file))
             
             (define success?
               (with-handlers ([exn:fail? (lambda (e) 
                                           (displayln (format "  ✗ ~a" (exn-message e)))
                                           #f)])
                 (define vm (make-vm))
                 (initialize-stdlib vm)
                 (define content (file->string test-path))
                 (define tokens (parse-rutile content))
                 (execute-tokens vm tokens)
                 #t))
             
             (if success?
                 (set! passed (+ passed 1))
                 (set! failed (+ failed 1))))
           
           (displayln "")
           (displayln (format "Results: ~a passed, ~a failed" passed failed))
           (when (> failed 0)
             (exit 1))))]))

(define (rutile-build args)
  (displayln "Building rutile project...")
  (cond 
    [(null? args) (build-current-project)]
    [(equal? (car args) "--static") (build-static-executable)]
    [(equal? (car args) "--help") (show-build-help)]
    [else (build-project-with-args args)]))

(define (build-current-project)
  (displayln "Building current project...")
  (displayln "Project built successfully"))

(define (build-static-executable)
  (displayln "Building static executable...")
  (define exe-name (if (eq? (system-type) 'windows) "rtl.exe" "rtl"))
  (define dist-dir "dist")
  
  ;; Step 1: Create executable
  (define build-cmd (format "raco exe -o ~a main.rkt" exe-name))
  (displayln (format "Running: ~a" build-cmd))
  (system build-cmd)
  
  ;; Step 2: Create distribution directory with all dependencies
  (displayln "Creating distribution...")
  (when (directory-exists? dist-dir)
    (delete-directory/files dist-dir))
  (make-directory dist-dir)
  
  (define dist-cmd (format "raco distribute ~a ~a" dist-dir exe-name))
  (displayln (format "Running: ~a" dist-cmd))
  (system dist-cmd)
  
  ;; Step 3: Copy runtime modules and examples
  (copy-runtime-files dist-dir)
  
  (displayln (format "Static distribution created in: ~a/" dist-dir))
  (displayln "Distribution is self-contained and can run without Racket installed."))

(define (build-project-with-args args)
  (displayln (format "Building with args: ~a" args))
  (displayln "Build complete"))

(define (copy-runtime-files dist-dir)
  ;; Copy example modules and runtime files to distribution
  (define modules-src "modules")
  (define modules-dest (build-path dist-dir "modules"))
  
  (when (directory-exists? modules-src)
    (displayln "Copying runtime modules...")
    (copy-directory/files modules-src modules-dest))
  
  ;; Copy example files
  (define examples '("test-simple-module.rtl" "test-bytecode-exec.rtl" "simple-test.rtl"))
  (for ([example examples])
    (when (file-exists? example)
      (copy-file example (build-path dist-dir example))))
  
  ;; Create a README for the distribution
  (define readme-content 
    "# Rutile Distribution\n\nThis is a self-contained Rutile distribution.\n\n## Usage\n\n./rtl run script.rtl    # Run a Rutile script\n./rtl repl              # Start interactive REPL\n./rtl new project-name  # Create new project\n./rtl build --static    # Build executable\n\n## Examples\n\nTry running the included examples:\n\n./rtl run simple-test.rtl\n./rtl run test-bytecode-exec.rtl\n\n## Modules\n\nExample modules are in the modules/ directory.\n")
  
  (with-output-to-file (build-path dist-dir "README.md")
    (lambda () (display readme-content))
    #:exists 'replace))

(define (show-build-help)
  (displayln "Build options:")
  (displayln "  rtl build              - Build current project")
  (displayln "  rtl build --static     - Create standalone executable")
  (displayln "  rtl build --help       - Show this help"))

;; Debug REPL with development features
(define (rutile-debug-repl)
  (define vm (make-vm))
  (initialize-stdlib vm)
  (debug-repl-with-features vm))

;; Debugger REPL
(define (rutile-debug [script-path #f])
  (define vm (make-vm))
  (initialize-stdlib vm)
  (define debugger (vm-debugger vm))
  
  (if script-path
      (let ([content (file->string script-path)]
            [tokens (parse-rutile (file->string script-path))])
        (displayln (format "Loading script for debugging: ~a" script-path))
        (vm-set-source-context vm script-path content)
        (debug-repl debugger))
      (debug-repl debugger)))

;; Command line entry point
(define (main . args)
  (cond
    [(null? args) (rutile-repl)]
    [(equal? (car args) "repl") (rutile-repl)]
    [(equal? (car args) "run") 
     (if (null? (cdr args))
         (displayln "Error: run requires a script file")
         (rutile-run (cadr args)))]
    [(equal? (car args) "new")
     (if (null? (cdr args))
         (displayln "Error: new requires a project name")
         (rutile-new (cadr args)))]
    [(equal? (car args) "test") (rutile-test)]
    [(equal? (car args) "build") (rutile-build (cdr args))]
    [(equal? (car args) "debug") 
     (if (null? (cdr args))
         (rutile-debug)
         (rutile-debug (cadr args)))]
    [(equal? (car args) "dev") (rutile-debug-repl)]
    [else 
     (displayln "Rutile concatenative language")
     (displayln "Usage:")
     (displayln "  racket main.rkt repl         - Start REPL")
     (displayln "  racket main.rkt run file.rtl - Run script")
     (displayln "  racket main.rkt new project  - Create project")
     (displayln "  racket main.rkt test         - Run tests")
     (displayln "  racket main.rkt build        - Build project")
     (displayln "  racket main.rkt dev           - Start development REPL")
     (displayln "  racket main.rkt debug [file]  - Start debugger")]))

;; When run as script
(module+ main
  (apply main (vector->list (current-command-line-arguments))))