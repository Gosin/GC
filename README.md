# Configurations for tools used by Gosin
Personal configuration

## Vim
* install Vundle ```git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim```
* run **deployConfigs** to apply configs. 
* install **Node.js** with **standard**, **eslint**, **htmlhint**.  
* install extra plugins to enable **eslint** with **standard**  
  ```npm install --save-dev eslint-config-standard eslint-plugin-standard eslint-plugin-promise eslint-plugin-import eslint-plugin-node```
* Add ```.eslintrc``` as needed.  
* YouCompleteMe plugin installation:
  * C-Family completetion: Install `CMake` . Add `--clang-completer`.
  * Javascript completion: Install `Node.js` and `npm`. Add `--js-completer`.
  * Rust completion: Install `Rust`. Add `--rust-completer`.

## Tmux
* install tmux plugin manager ```git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm```
