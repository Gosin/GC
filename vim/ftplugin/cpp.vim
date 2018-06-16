" Vim configuration file for C++
"
" set UTF-8 encoding
setlocal enc=utf-8
setlocal fenc=utf-8
setlocal termencoding=utf-8

" use indentation of previous line
setlocal autoindent
" use intelligent indentation for C
setlocal smartindent
" configure tabwidth and insert spaces instead of tabs
setlocal tabstop=2		" tab width is 2 spaces
setlocal shiftwidth=2	" indent also with 2 spaces
setlocal expandtab		" expand tabs to spaces

" wrap lines at 100 chars.
setlocal textwidth=100

" highlight matching braces
setlocal showmatch

" set format options with exceptions for command o
setlocal formatoptions=crql
