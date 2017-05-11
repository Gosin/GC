" Vim Configuration File for Gosin
" 2017 05 09
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
Plugin 'ctrlpvim/ctrlp.vim'

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

" HTML5 syntax highlighting
Plugin 'othree/html5.vim'

" JavaScript syntax highlighting 
Plugin 'pangloss/vim-javascript'

" JavaScript library synstax highlighting
Plugin 'othree/javascript-libraries-syntax.vim'

" Markdown syntax highlighting
Plugin 'plasticboy/vim-markdown'

" Mathematica syntax highlighting
Plugin 'rsmenon/vim-mathematica'

" Vue highlight
Plugin 'posva/vim-vue'

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

" Set textwidth with highlight column
set colorcolumn=80,100

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
" only use htmlhint to lint html file since tidy doesn't support HTML5.
let g:ale_linters = {
    \ 'html': ['htmlhint'],
    \ 'javascript': ['eslint'],
    \ }

" setup library used for javascript
let g:used_javascript_libs = 'jquery,vue'

" settings for ctrlp
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_open_new_file = 'v'
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = {
    \ 'dir': '\v[\/](node_modules|semantic)$',
    \ }
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn|project|DS_Store)&'

" getting path of npm bin function
" echo join(split(system('npm bin')) + ['eslint'], '/')
au BufRead,BufNewFile *.wl set filetype=mma
