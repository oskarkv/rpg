let sm = "autocmd Syntax clojure syntax match"
let nsq = '''\m\([a-z0-9A-Z.\-_]\+\/\)\?'
let sym_rest = '\m[a-z0-9A-Z.+\-*_!?=]*'
let endq = ''''
let macro = " clojureMacro "
let fn = " clojureFunc "
let define = " clojureDefine "
let cond = " clojureCond "
let exception = " clojureException "

function! CljHL(type, name)
  execute g:sm . a:type . g:nsq . a:name . g:endq
endfunction

function! CljHL_only_beginning(type, name)
  execute g:sm . a:type . g:nsq . a:name . g:sym_rest . g:endq
endfunction

augroup clojure_settings
  autocmd!
  " 80 cols marker
  autocmd FileType clojure :let &colorcolumn=81

  " Syntax highlighting
  autocmd Syntax clojure syntax keyword clojureDefine let def letfn
  autocmd Syntax clojure syntax keyword clojureRepeat loop while-let
  autocmd Syntax clojure syntax keyword clojureCond if
  autocmd Syntax clojure syntax keyword clojureMacro assert-args

  call CljHL(fn, 'dissoc-in')
  call CljHL(fn, 'partial\*')
  call CljHL(fn, 'flip')
  call CljHL(fn, 'rec==')
  call CljHL(fn, 'fmap')
  call CljHL(cond, 'condf')
  call CljHL(exception, 'throw-error')
  call CljHL(macro, 'call-update-fns')
  call CljHL(macro, 'error-printing-future')
  call CljHL(macro, 'start-new-thread')
  call CljHL_only_beginning(define, 'def')
  call CljHL_only_beginning(macro, 'with-')

  " Remove trailing whitespace
  autocmd BufWritePre *.clj :%s/\s\+$//e
augroup END

" Indentation
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^while-let$',
            \'call-update-fns', 'start-new-thread', 'take-at-least-ms', 'condf']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,deftype-'
let g:clojure_align_multiline_strings = 1
