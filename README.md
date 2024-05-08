# Configurations for tools used by Gosin
Personal configuration

# Rime

* Copy `rime/double_pinyin_flypy.schema.yaml` to rime config directory to add 小鹤双拼.
  * https://github.com/rime/rime-double-pinyin
* Copy `rime/default.custom.yaml` to rime config directory to update input methods.

## Emacs

* Copy `init.el` , `emacs-custom.el`, and `goer.el` to  default config directory`~/.emacs.d/`

* Install packages for the first time.

  * From: https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
  ```
  (custom-set-variables
  '(package-selected-packages
      '(org js2-mode yasnippet use-package org-mac-link org-notify scala-mode yaml-mode exec-path-from-shell magit flymake-shellcheck flycheck-ledger flycheck-clang-tidy flycheck-clang-analyzer flycheck command-log-mode which-key tree-sitter-langs tree-sitter lua-mode glsl-mode ob-rust htmlize pinentry cnfonts org-bullets helm paredit sml-mode ycmd solarized-theme rust-mode rainbow-delimiters markdown-mode company cmake-mode)))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install-selected-packages)
  ```
## GPG Configuration
  * Specify `pinentry`:
    `pinentry-program path/to/pinentry`
    https://stackoverflow.com/questions/39494631/gpg-failed-to-sign-the-data-fatal-failed-to-write-commit-object-git-2-10-0

## Tmux
* install tmux plugin manager.
  `git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`
  * Install `resurrect` and `sensible` plugins.
