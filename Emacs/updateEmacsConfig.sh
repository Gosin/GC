#!/bin/zsh
cp ~/.emacs.d/init.el ./init.el
echo "init.el copied"
cp ~/.emacs.d/emacs-custom.el ./emacs-custom.el
echo "emacs-custom.el copied"
cp -R ~/.emacs.d/snippets/ ./snippets/
echo "yas snippets copied"
cp ~/.emacs.d/goer.el ./goer.el
echo "goer.el copied"
