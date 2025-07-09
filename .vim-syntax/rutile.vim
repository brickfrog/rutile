" Vim syntax file for Rutile
" Language: Rutile
" Maintainer: Rutile Team
" Latest Revision: 2024

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword rutileKeyword : ; if then else while times each map filter fold try catch throw finally
syn keyword rutileBuiltin dup drop swap rot over pick roll depth 2dup 2drop 2swap nip tuck 3dup 3drop
syn keyword rutileMath + - * / = < > <= >= != and or not sin cos tan log exp sqrt pow abs min max random pi e
syn keyword rutileIO read-file write-file append-file file-exists? delete-file read-line write-line
syn keyword rutileStack print-top clear-stack show-words show-stack
syn keyword rutileLLM llm-call llm-default-config llm-stream llm-multi-turn
syn keyword rutileAgent spawn-agent agent-send agent-receive agent-kill agent-list supervise-agent start-supervisor stop-supervisor agent-info agent-health
syn keyword rutileWebhook webhook-listen webhook-handler webhook-stop webhook-server
syn keyword rutileHotReload hot-reload watch-file auto-reload reload-on-change
syn keyword rutileJSON json-encode json-decode json-get json-set json-keys
syn keyword rutileCrypto sha256 md5 base64-encode base64-decode uuid hash-equal
syn keyword rutileRegex regex-match regex-replace regex-split regex-find-all regex-test
syn keyword rutileHTTP http-get http-post async-http-get async-http-post chunked-response stream-response
syn keyword rutileError error-message error-type
syn keyword rutileVar let with-locals bind unbind get-local set-local
syn keyword rutileType string? number? boolean? list? null? empty?
syn keyword rutileList cons car cdr length concat split substr
syn keyword rutileTime now time date timestamp sleep
syn keyword rutileTest assert test-passed test-failed

" Comments
syn match rutileComment "#.*$"
syn region rutileBlockComment start="(" end=")" contains=rutileBlockComment

" Strings
syn region rutileString start='"' end='"' skip='\\"'

" Numbers
syn match rutileNumber "\<-\?\d\+\>"
syn match rutileFloat "\<-\?\d\+\.\d\+\>"

" Quotations
syn region rutileQuotation start="\[" end="\]" contains=ALL

" Maps
syn region rutileMap start="{" end="}" contains=ALL

" Webhook sigils
syn match rutileWebhookSigil "@\w\+"

" Highlighting
hi def link rutileKeyword Keyword
hi def link rutileBuiltin Function
hi def link rutileMath Operator
hi def link rutileIO Statement
hi def link rutileStack Special
hi def link rutileLLM Function
hi def link rutileAgent Function
hi def link rutileWebhook Function
hi def link rutileHotReload Function
hi def link rutileJSON Function
hi def link rutileCrypto Function
hi def link rutileRegex Function
hi def link rutileHTTP Function
hi def link rutileError Exception
hi def link rutileVar Identifier
hi def link rutileType Type
hi def link rutileList Function
hi def link rutileTime Function
hi def link rutileTest Function
hi def link rutileComment Comment
hi def link rutileBlockComment Comment
hi def link rutileString String
hi def link rutileNumber Number
hi def link rutileFloat Float
hi def link rutileQuotation Special
hi def link rutileMap Special
hi def link rutileWebhookSigil Macro

let b:current_syntax = "rutile"