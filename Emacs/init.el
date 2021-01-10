;; Gosin's Emacs Configuration 2021/01/09
;; add ELPA and MELPA package repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))

;; user solarized dark color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized_theme")
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; define three useful commands(from org guide)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(custom-set-variables
 '(global-display-line-numbers-mode t) ; display line numbers
 '(apropos-sort-by-scores t)) ; sort apropos results by relevance

;; store backup and autosave files to temporary directories
(setq backup-director-alist
      '((".*" . ,temporary-file-directory)))
