" $ cp oc.vim ~/.vim/syntax/oc.vim
" $ grep '.oc' ~/.vimrc
" autocmd BufNewFile,BufRead *.oc setlocal filetype=oc

if exists("b:current_syntax")
    finish
endif

syn match Operator  "[(){}\[\];\\@|?=+\-*/%]"
syn match Number    "\<[0-9]\+\>"

syn match ocSpecial contained "\\\(n\|\"\|\\\)"
syn region String start=+"+ skip=+\\"+ end=+"+ contains=ocSpecial

hi def link ocSpecial SpecialChar

" NOTE: See `http://vimdoc.sourceforge.net/htmldoc/syntax.html`.
syn keyword Statement
    \ return
    \ let
    \ set
    \ seta
    \ new
    \ into
    \ switch
syn keyword Function
    \ entry
    \ printf
    \ alloc
    \ free
    \ deref
    \ mask
    \ unmask
    \ print_stack
syn keyword Type
    \ Int
    \ Str

syn match Function "child+"
syn match Function "child-"

syn match Comment "#.*" contains=Todo
syn keyword Todo FIXME NOTE TODO contained

let b:current_syntax = "oc"
