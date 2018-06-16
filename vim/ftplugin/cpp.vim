" Vim configuration file for C++

" Preventing vim built-in cpp.vim to override options.
let b:did_ftplugin = 1

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

" do not format line inserted by o 
setlocal formatoptions-=o
