" Don't load another plugin for this buffer.
" Preventing Vim load its own c.vim to override the settings.
let b:did_ftplugin = 1

" do not format line inserted by o
setlocal formatoptions-=o
