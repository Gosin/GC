;; Gosin's Emacs Configuration 2021/06/04
;; specify file path of customization
(setq custom-file "~/.emacs.d/emacs-custom.el")
;; load customization file
(load custom-file)

;; use solarized dark color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized_theme")
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

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

;; remap keys for iTerm 2
;; use cat -v or ctrl-v to show the sequence in iTerm 2.
;; replace ^[ with \e
(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])
