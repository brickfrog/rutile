;;; rutile-mode.el --- Major mode for Rutile programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Rutile Team

;; Author: Rutile Team
;; Keywords: languages
;; Version: 0.1.0

;;; Commentary:

;; This package provides a major mode for editing Rutile source code.
;; Rutile is a concatenative programming language with LLM integration.

;;; Code:

(defvar rutile-mode-hook nil
  "Hook run when entering Rutile mode.")

(defvar rutile-keywords
  '(":" ";" "if" "then" "else" "while" "times" "each" "map" "filter" "fold" 
    "try" "catch" "throw" "finally" "let" "with-locals" "bind" "unbind")
  "Rutile keywords.")

(defvar rutile-builtin-words
  '("dup" "drop" "swap" "rot" "over" "pick" "roll" "depth" "2dup" "2drop" "2swap"
    "nip" "tuck" "3dup" "3drop" "print-top" "clear-stack" "show-words" "show-stack")
  "Rutile builtin stack manipulation words.")

(defvar rutile-math-words
  '("+" "-" "*" "/" "=" "<" ">" "<=" ">=" "!=" "and" "or" "not"
    "sin" "cos" "tan" "log" "exp" "sqrt" "pow" "abs" "min" "max" "random" "pi" "e")
  "Rutile math operations.")

(defvar rutile-llm-words
  '("llm-call" "llm-default-config" "llm-stream" "llm-multi-turn")
  "Rutile LLM integration words.")

(defvar rutile-agent-words
  '("spawn-agent" "agent-send" "agent-receive" "agent-kill" "agent-list"
    "supervise-agent" "start-supervisor" "stop-supervisor" "agent-info" "agent-health")
  "Rutile agent management words.")

(defvar rutile-io-words
  '("read-file" "write-file" "append-file" "file-exists?" "delete-file" "read-line" "write-line")
  "Rutile I/O operations.")

(defvar rutile-font-lock-keywords
  `(
    ;; Comments
    ("#.*$" . font-lock-comment-face)
    ("(\\([^)]*\\))" . font-lock-comment-face)
    
    ;; Strings
    ("\"\\([^\"\\\\]\\|\\\\.\\)*\"" . font-lock-string-face)
    
    ;; Numbers
    ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    
    ;; Keywords
    (,(regexp-opt rutile-keywords 'words) . font-lock-keyword-face)
    
    ;; Builtin words
    (,(regexp-opt rutile-builtin-words 'words) . font-lock-builtin-face)
    
    ;; Math words
    (,(regexp-opt rutile-math-words 'words) . font-lock-function-name-face)
    
    ;; LLM words
    (,(regexp-opt rutile-llm-words 'words) . font-lock-function-name-face)
    
    ;; Agent words
    (,(regexp-opt rutile-agent-words 'words) . font-lock-function-name-face)
    
    ;; I/O words
    (,(regexp-opt rutile-io-words 'words) . font-lock-function-name-face)
    
    ;; Webhook sigils
    ("@\\w+" . font-lock-preprocessor-face)
    
    ;; Quotations
    ("\\[\\|\\]" . font-lock-variable-name-face)
    
    ;; Maps
    ("{\\|}" . font-lock-variable-name-face)
    )
  "Font lock keywords for Rutile mode.")

(defvar rutile-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "(" table)
    (modify-syntax-entry ?\) ")" table)
    
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    
    ;; Brackets
    (modify-syntax-entry ?\[ "(" table)
    (modify-syntax-entry ?\] ")" table)
    (modify-syntax-entry ?\{ "(" table)
    (modify-syntax-entry ?\} ")" table)
    
    table)
  "Syntax table for Rutile mode.")

(defun rutile-indent-line ()
  "Indent current line as Rutile code."
  (interactive)
  (let ((indent-level 0))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^\\s-*;")
        (setq indent-level -2)))
    (save-excursion
      (forward-line -1)
      (when (looking-at "^\\s-*:")
        (setq indent-level 2)))
    (indent-line-to (max 0 (+ (current-indentation) indent-level)))))

;;;###autoload
(define-derived-mode rutile-mode prog-mode "Rutile"
  "Major mode for editing Rutile source code."
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(rutile-font-lock-keywords))
  (setq-local indent-line-function 'rutile-indent-line)
  (setq-local syntax-table rutile-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rtl\\'" . rutile-mode))

(provide 'rutile-mode)

;;; rutile-mode.el ends here