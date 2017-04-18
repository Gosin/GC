" Vim Configuration File for Gosin
" 2017 04 18
" Practicing my skill.

" Plugins
" ===============
" Setup Vundle For Package Management
"
" Vundle begins here, turnoff filetype temporarily
filetype off
" Set the runtime path to inlcude Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, requried
Plugin 'VundleVim/Vundle.vim'

" # Themes
Plugin 'altercation/vim-colors-solarized'

" Functionality

" ctrlp for file fuzzy search
Plugin 'kien/ctrlp.vim'

" Ack.vim uses ack to search within files
Plugin 'mileszs/ack.vim'

" Airline provides a stylish appearance for the styleline
Plugin 'bling/vim-airline'

" NERD Commenter
Plugin 'scrooloose/nerdcommenter'

" Emmet
Plugin 'mattn/emmet-vim'

" Fugitive for enhanced git plugin
Plugin 'tpope/vim-fugitive'

" Asynchronous Linter Engine
Plugin 'w0rp/ale'

" End Vundle
call vundle#end()
" Vundle ended so enable filetypes
filetype plugin indent on

" Show statusline always so that airline is always available.
set laststatus=2

" Use Vim settings without concerning Vi.
set nocompatible

" Make backspace behave in a normal manner.
set backspace=indent,eol,start

" Switch syntax highlight on
syntax on
set background=dark
colorscheme solarized

" Show line numbers
set number

" Set indentation
set tabstop=4
set expandtab
set shiftwidth=4
set autoindent smartindent

" Set auto wrap
set wrap

" Set syntax highlight for CMake
au BufNewFile,BufRead CMakeLists.txt set filetype=cmake
" Allow hidden buffers, don't limit to 1 file per window/split

" Edit vimrc file
:nnoremap <leader>ev :split $MYVIMRC<cr>

" Add hightlight to search pattern and next match
set hlsearch incsearch

" Abbreviations
" personal email
iabbrev cl@ canling0@gmail.com
" working email
iabbrev xg@ xinguo@wolfram.com

" Recommended settings for NERDCommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code
" indentation
let g:NERDDefaultAlign = 'left'
" set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1
" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**', 'right': '*/' } }
" Allow commenting and inverting empty lines (useful when commenting a region
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespaces when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" settings for ALE
" add ALE to statusline
set statusline+=%{ALEGetStatusline()}

" set tabs for javascript
autocmd Filetype javascript setlocal tabstop=2 expandtab shiftwidth=2 softtabstop=2 
