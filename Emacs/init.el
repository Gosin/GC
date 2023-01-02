;; Gosin's Emacs Configuration 2021/08/20

;; start emacs as a server
(server-start)

;; Set up pin entry for signing commits (See EasyPG Assistant for more information)
;; Start the server to make it working
(pinentry-start)

;; Import shell environment to Emacs shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; specify file path of customization
(setq custom-file "~/.emacs.d/emacs-custom.el")
;; load customization file
(load custom-file)

;; use solarized dark color theme
(load-theme 'solarized-dark t)
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Change the size of markdown-mode headlines (off by default)
(setq solarized-scale-markdown-headlines t)

;; enable directional window selection
(windmove-default-keybindings)

;; enable paredit mode
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;; enable rainbow-delimiter mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; define three useful commands(from org guide)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; save org clock history across sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; set org-mode default capture note file
(setq org-default-notes-file (concat org-directory "/note.org"))

;; enable org-tempo by default
(require 'org-tempo)

;; enable org-bullets-mode by default
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda () (org-bullets-mode 1)))

;; Replace dabbrev with hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; remap keys for iTerm 2
;; use cat -v or ctrl-v to show the sequence in iTerm 2.
;; ^ is Ctrl, ^[ is Alt/Meta
;; replace ^[ with \e
;; Terminal-based Emacs didn't handle key bindings properly, use GUI-based Emacs onwards
