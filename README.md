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
  * C-Family completetion: Install `CMake` . Add `--clang-completer` and `--clangd-completer`(https://clangd.llvm.org/installation.html) .
  * Add softlink of `clangd` and `clang-tidy` to user binary directory.
  * Javascript completion: Install `Node.js` and `npm`. Add `--ts-completer`.
  * Rust completion: Install `Rust`. Add `--rust-completer`.
* Adding clang-format and clang-tidy to the environment.
  * `ln -s path/to/clang-format /usr/local/bin/clang-format`

## Emacs
* Copy `init.el` to `~/.emacs.d/init.el`
* Packages:
  * Rainbow-Delimiters: https://github.com/Fanael/rainbow-delimiters


## Tmux
* install tmux plugin manager ```git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm```

