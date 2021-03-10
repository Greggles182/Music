" Vim syntax file
" Filename: pmw.vim
" Version: 1.0
" Maintainer: Elias Dorneles da Silveira Junior
" URL: http://www.inf.ufsm.br/~eljunior/pmw/pmw.vim
" Installation: Add in your ~/.vimrc the following line:
"   au BufNewFile,BufRead *.pmw set ft=pmw
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syn clear
syn case ignore

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"MISC:
syn keyword pmwTodo    TODO FIXME XXX contained
syn match   pmwComment '^@.*$' contains=pmwTodo 
syn match   pmwComment '[|]'
syn match   pmwNumber  '\<\d\+\([,.]\d\+\)\{,1}\>'
syn match   pmwPercent '\<\d\+\([,.]\d\+\)\{,1}%'
" mark the blank unneeded
syn match   pmwBlank   '\s\+$'
syn match   pmwBar     '^\s*[_=-]\{20,}\s*$'
syn match   pmwSpecial '[][(){}®]'
syn match   pmwSpecial '\<\(US\|R\)\$'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"URL EMAIL:
syn match pmwEmail '\<[A-Za-z0-9_.-]\+@\([A-Za-z0-9_-]\+\.\)\+[A-Za-z]\{2,4}\>\(?[A-Za-z0-9%&=+.,@*_-]\+\)\='
syn match pmwUrl   '\<\(\(https\=\|ftp\|news\|telnet\|gopher\|wais\)://\([A-Za-z0-9._-]\+\(:[^ @]*\)\=@\)\=\|\(www[23]\=\.\|ftp\.\)\)[A-Za-z0-9%._/~:,=$@-]\+\>/*\(?[A-Za-z0-9/%&=+.,@*_-]\+\)\=\(#[A-Za-z0-9%._-]\+\)\='

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"FONT BEAUTIFIERS:
syn match   pmwBold       '\*[^*[:blank:]].\{-}\*'hs=s+1,he=e-1
syn match   pmwMonospace  "`[^`]\+`"hs=s+1,he=e-1
" max: two lines
syn match   pmwParentesis "([^)]\+\(\n\)\=[^)]*)" contains=pmwUrl,pmwEmail
syn match   pmwQuotes     '"[^"]\+\(\n\)\=[^"]*"'hs=s+1,he=e-1
" max: two words
syn match   pmwQuotes     "'\w\+ \?\w\+'"hs=s+1,he=e-1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" color definitions (specific)
hi pmwBar         term=bold        cterm=bold        gui=bold
hi pmwBold        term=bold        cterm=bold        gui=bold
hi pmwItalic      term=italic      cterm=italic      gui=italic

" color definitions (using Vim defaults)
hi link pmwComment      Comment
hi link pmwQuotes       String
hi link pmwBlank        Error
hi link pmwNumber       Number
hi link pmwPercent      Number
hi link pmwTodo         Todo
hi link pmwEmail        PreProc
hi link pmwUrl          PreProc
hi link pmwMonospace    Special
hi link pmwSpecial      Special
hi link pmwParentesis   Statement
"
let b:current_syntax = 'pmw'
" vim:tw=0:et
