# Configurations for tools used by Gosin
Personal configuration

## Emacs
* Copy `init.el` to `~/.emacs.d/init.el`

* Package Installation Instructions:

  * ```elisp
    M-x list-packages
    ```

  * `i` to mark for installation.

  * `u` to unmark.

  * `x` to execute.

  * ```
    ;; refresh package contents
    package-refresh-content
    ```
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

## GPG Conf
  * Specify `pinentry`:
    `pinentry-program path/to/pinentry`
    https://stackoverflow.com/questions/39494631/gpg-failed-to-sign-the-data-fatal-failed-to-write-commit-object-git-2-10-0

## Tmux
* install tmux plugin manager.
  `git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`
