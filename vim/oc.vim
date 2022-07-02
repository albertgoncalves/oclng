" $ cp oc.vim ~/.vim/syntax/oc.vim
" $ grep '.oc' ~/.vimrc
" autocmd BufNewFile,BufRead *.oc setlocal filetype=oc

if exists("b:current_syntax")
    finish
endif

syn match Comment   "#.*$"
syn match Operator  "[(){}]"
syn match Number    "\<[0-9]\+\>"

syn match ocSpecial contained "\\\(n\|\"\|\\\)"
syn region String start=+"+ skip=+\\"+ end=+"+ contains=ocSpecial

hi def link ocSpecial SpecialChar

" NOTE: See `http://vimdoc.sourceforge.net/htmldoc/syntax.html`.
syn keyword Statement
    \ if
    \ else
    \ let
    \ unpack
syn keyword Keyword
    \ fflush
    \ printf
    \ pack

let b:current_syntax = "oc"
