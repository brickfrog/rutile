#lang racket/base

(define (builtin-word? name)
  (member name '("dup" "drop" "swap" "rot" "over" "+" "-" "*" "/" "="
                 "<" ">" "<=" ">=" "!=" 
                 "and" "or" "not"
                 "if" "then" "else"
                 "true" "false"
                 "pick" "roll" "depth" "2dup" "2drop" "2swap"
                 "nip" "tuck" "3dup" "3drop"
                 "concat" "split" "length" "substr" "string?" "number?" "boolean?"
                 "list?" "null?" "empty?" "cons" "car" "cdr"
                 "map" "filter" "fold" "each" "times" "while"
                 "read-file" "write-file" "append-file" "file-exists?" "delete-file"
                 "read-line" "write-line"
                 "sin" "cos" "tan" "asin" "acos" "atan" "atan2"
                 "log" "log10" "exp" "sqrt" "pow" "abs" "floor" "ceiling" "round"
                 "min" "max" "random" "pi" "e"
                 "now" "time" "date" "timestamp" "sleep"
                 "print-top" "clear-stack" "show-words" "show-stack"
                 "http-get" "http-post" "json-parse" "json-stringify"
                 "llm-call" "llm-default-config" "llm-stream" "llm-multi-turn"
                 "assert" "test-passed" "test-failed"
                 "let" "with-locals" "bind" "unbind" "get-local" "set-local"
                 "http-server" "http-handler" "http-listen" "http-request" "http-response"
                 "async-http-get" "async-http-post" "chunked-response" "stream-response"
                 "regex-match" "regex-replace" "regex-split" "regex-find-all" "regex-test"
                 "try" "catch" "throw" "error-message" "error-type" "finally"
                 "json-encode" "json-decode" "json-get" "json-set" "json-keys"
                 "sha256" "md5" "base64-encode" "base64-decode" "uuid" "hash-equal"
                 "webhook-listen" "webhook-handler" "webhook-stop" "webhook-server"
                 "hot-reload" "watch-file" "auto-reload" "reload-on-change"
                 "spawn-agent" "agent-send" "agent-receive" "agent-kill" "agent-list"
                 "supervise-agent" "start-supervisor" "stop-supervisor" "agent-info" "agent-health")))

(displayln (builtin-word? "json-encode"))
(displayln (builtin-word? "agent-list"))
(displayln (builtin-word? "start-supervisor"))