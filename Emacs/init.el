;; Gosin's Emacs Configuration 2021/01/09
;; add ELPA and MELPA package repos
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;; user solarized dark color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized_theme")
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; enable rainbow-delimiter mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; define three useful commands(from org guide)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-sort-by-scores t)
 '(backup-directory-alist `((".*" \, temporary-file-directory)))
 '(global-display-line-numbers-mode t)
 '(org-log-done 'time)
 '(org-log-into-drawer "LOGBOOK")
 '(package-selected-packages
   '(paredit sml-mode ycmd solarized-theme rust-mode rainbow-delimiters markdown-mode magit company cmake-mode)))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
