#lang racket/base

(require racket/cmdline)
(require "rutile/main.rkt")

(define (main)
  (command-line
   #:program "rtl"
   #:once-each
   [("-v" "--version") "Show version"
    (displayln "rutile 0.1.0")
    (exit 0)]
   #:args (subcommand . args)
   (case subcommand
     [("repl") (rutile-repl)]
     [("run") (if (null? args)
                  (error "Usage: rtl run <script.rtl>")
                  (rutile-run (car args)))]
     [("new") (if (null? args)
                  (error "Usage: rtl new <project-name>")
                  (rutile-new (car args)))]
     [("test") 
      (if (and (not (null? args)) (string=? (car args) "--property"))
          (rutile-test #:property-based #t)
          (rutile-test))]
     [("build") (rutile-build args)]
     [else (error "Unknown subcommand:" subcommand)])))

(main)