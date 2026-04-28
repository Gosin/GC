(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-sort-by-scores t)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs_backups")))
 '(column-number-mode t)
 '(debug-on-error t nil nil "Turn on this flag to help learning emacs.")
 '(delete-old-versions t)
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
 '(eshell-mode-hook '((lambda nil (setenv "TERM" "xterm-256color"))))
 '(fido-mode t)
 '(global-display-line-numbers-mode t)
 '(global-visual-line-mode t)
 '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido) "Enable ido-mode for all buffers and files")
 '(indent-tabs-mode nil)
 '(kept-new-versions 5)
 '(lua-documentation-url "http://www.lua.org/manual/5.4/manual.html")
 '(org-structure-template-alist
   '(("a" . "export ascii") ("c" . "center") ("C" . "comment")
     ("e" . "example") ("E" . "export") ("h" . "export html")
     ("l" . "export latex") ("q" . "quote") ("s" . "src")
     ("v" . "verse")))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("nonGnuElpa" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(async clang-format cmake-mode corfu company cond-let consult
           consult-org-roam corfu ef-themes emacsql envrc
           exec-path-from-shell flymake flymake-ruff gptel helm
           helm-core inheritenv jinja2-mode js2-mode llama lua-mode
           magit magit-section marginalia markdown-mode modus-themes
           no-littering orderless org org-bullets org-contrib
           org-mac-link org-notify org-roam org-superstar paredit
           pinentry posframe rainbow-delimiters rust-mode transient
           treesit-auto vertico wfnames with-editor yaml yasnippet
           yasnippet-snippets))
 '(standard-indent 4)
 '(tab-always-indent nil)
 '(tab-width 4)
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
