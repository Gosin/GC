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

;; enable org-bullets-mode by default
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda () (org-bullets-mode 1)))

;; remap keys for iTerm 2
;; use cat -v or ctrl-v to show the sequence in iTerm 2.
;; ^ is Ctrl, ^[ is Alt/Meta
;; replace ^[ with \e
;; Terminal-based Emacs didn't handle key bindings properly, use GUI-based Emacs onwards
