(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-sort-by-scores t)
 '(backup-directory-alist '(("." . "~/.emacs_backups")))
 '(dired-clean-confirm-killing-deleted-buffers nil nil nil "don't ask whether to kill")
 '(dired-dwim-target t nil nil "guess target destination")
 '(dired-listing-switches "-alt" nil nil "Set flags as show hidden, long listing, sort by date")
 '(dired-recursive-copies 'always nil nil "copy recursively without asking")
 '(dired-recursive-deletes 'top nil nil "delete recursively and only confirm top-level directories")
 '(display-time-24hr-format t nil nil "Display clock time in 24 Hour format.")
 '(display-time-day-and-date t nil nil "Display clock time and date altogether")
 '(display-time-format "%Y-%m-%d(%a) %H:%M:%S ")
 '(display-time-interval 1)
 '(display-time-mode t nil nil "Display clock time in mode line.")
 '(epg-pinentry-mode 'loopback)
 '(exec-path
   '("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/Library/TeX/texbin" "/usr/local/bin" "/usr/local/opt/llvm/bin/"))
 '(fido-mode t)
 '(global-display-line-numbers-mode t)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(latex-run-command "pdflatex")
 '(lua-documentation-url "http://www.lua.org/manual/5.4/manual.html")
 '(org-agenda-files '("~/Documents/Dropbox/Org/"))
 '(org-agenda-include-diary t)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (css . t)
     (gnuplot . t)
     (js . t)
     (latex . t)
     (ledger . t)
     (octave . t)
     (python . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))
 '(org-capture-templates
   '(("b" "添加一本正在阅读的图书" entry
      (file+headline "~/Documents/Dropbox/Org/books.org" "Bookshelf")
      "* READING %?%U" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("t" "Add an TODO item" entry
      (file "~/Documents/Dropbox/Org/note.org")
      "* TODO %?%U%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("w" "Add a Wyatt task" entry
      (file+headline "~/Documents/Dropbox/Org/wyatterp_wolfram.org" "Backlogs")
      "* TODO %?" :empty-lines-after 1)))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Documents/Dropbox/Org/note.org")
 '(org-directory "~/Documents/Dropbox/Org")
 '(org-hide-emphasis-markers t)
 '(org-log-done 'time)
 '(org-log-into-drawer "LOGBOOK")
 '(org-refile-targets '((org-agenda-files :maxlevel . 6)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("nonGnuElpa" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(tree-sitter-indent tree-sitter lua-mode glsl-mode eglot ob-rust htmlize pinentry cnfonts org-bullets helm paredit sml-mode ycmd solarized-theme rust-mode rainbow-delimiters markdown-mode magit company cmake-mode))
 '(standard-indent 4)
 '(tab-always-indent nil)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
