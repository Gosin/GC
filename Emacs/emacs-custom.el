(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-sort-by-scores t)
 '(backup-directory-alist '(("." . "~/.emacs_backups")))
 '(column-number-mode t)
 '(debug-on-error t nil nil "Turn on this flag to help learning emacs.")
 '(dired-auto-revert-buffer t nil nil "Always update Dired buffer when revisiting.")
 '(dired-clean-confirm-killing-deleted-buffers nil nil nil "don't ask whether to kill")
 '(dired-create-destination-dirs 'ask nil nil "Whether to create directory in path if missing")
 '(dired-dwim-target t nil nil "guess target destination")
 '(dired-listing-switches "-alt" nil nil "Set flags as show hidden, long listing, sort by date")
 '(dired-recursive-copies 'always nil nil "How to confirm copying nested directories.")
 '(dired-recursive-deletes 'top nil nil "delete recursively and only confirm top-level directories")
 '(dired-use-ls-dired 'unspecified)
 '(display-time-24hr-format t nil nil "Display clock time in 24 Hour format.")
 '(display-time-day-and-date t nil nil "Display clock time and date altogether")
 '(display-time-format "%Y-%m-%d(%a) %H:%M:%S ")
 '(display-time-interval 1)
 '(display-time-mode t nil nil "Display clock time in mode line.")
 '(epg-pinentry-mode 'loopback)
 '(exec-path
   '("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/Library/TeX/texbin" "/usr/local/bin" "/usr/local/opt/llvm/bin/" "/Users/gosin/.cargo/bin" nil))
 '(fido-mode t)
 '(global-display-line-numbers-mode t)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido) "Enable ido-mode for all buffers and files")
 '(indent-tabs-mode nil)
 '(latex-run-command "pdflatex")
 '(lua-documentation-url "http://www.lua.org/manual/5.4/manual.html")
 '(org-agenda-files
   '("/Users/gosin/Documents/Org/TAOCP.org" "/Users/gosin/Documents/Org/gosin.org" "/Users/gosin/Documents/Org/home.org" "/Users/gosin/Documents/Org/note.org" "/Users/gosin/Documents/Org/wyatterp_wolfram.org"))
 '(org-agenda-include-diary t)
 '(org-archive-save-context-info '(time file category todo itags olpath))
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
   '(("b" "添加一本图书")
     ("m" "添加一部影视作品")
     ("mt" "添加一部想看的影视作品" entry
      (file "~/Documents/Org/movies.org")
      "* TODO %?
  %U" :prepend t)
     ("md" "添加一部看过的电影" entry
      (file "~/Documents/Org/movies.org")
      "* DONE %?
  %U")
     ("mi" "添加一部正在看的电视剧" entry
      (file "~/Documents/Org/movies.org")
      "* WATCHING %?
  %U" :prepend t empty-lines-after 1)
     ("bd" "添加一本读过的图书" entry
      (file "~/Documents/Org/books.org")
      "* DONE %?
  %U")
     ("bt" "添加一本想读的图书" entry
      (file "~/Documents/Org/books.org")
      "* TODO %?
  %U")
     ("bi" "添加一本正在阅读的图书" entry
      (file "~/Documents/Org/books.org")
      "* READING %?
  %U" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("t" "Add an TODO item" entry
      (file "~/Documents/Org/note.org")
      "* TODO %?%U%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("w" "Add a Wyatt task" entry
      (file+headline "~/Documents/Org/wyatterp_wolfram.org" "Backlogs")
      "* TODO %?" :empty-lines-after 1)))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Documents/Dropbox/Org/note.org")
 '(org-directory "~/Documents/Org")
 '(org-hide-emphasis-markers t)
 '(org-log-done 'time)
 '(org-log-into-drawer "LOGBOOK")
 '(org-modules
   '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus org-habit org-id ol-info ol-irc ol-mhe org-protocol ol-rmail org-tempo ol-w3m ol-eshell ol-git-link org-mac-link org-notify))
 '(org-pretty-entities t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets '((org-agenda-files :maxlevel . 6)))
 '(org-use-sub-superscripts '{})
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("nonGnuElpa" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(js2-mode yasnippet use-package org-mac-link org-notify scala-mode yaml-mode exec-path-from-shell magit flymake-shellcheck flycheck-ledger flycheck-clang-tidy flycheck-clang-analyzer flycheck command-log-mode which-key tree-sitter-langs tree-sitter lua-mode glsl-mode ob-rust htmlize pinentry cnfonts org-bullets helm paredit sml-mode ycmd solarized-theme rust-mode rainbow-delimiters markdown-mode company cmake-mode))
 '(standard-indent 4)
 '(tab-always-indent nil)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :width normal :family "Berkeley Mono"))))
 '(cursor ((t (:background "yellow1" :foreground "#002b36" :inverse-video t)))))
