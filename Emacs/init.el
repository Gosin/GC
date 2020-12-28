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

;; display line numbers
(custom-set-variables '(global-display-line-numbers-mode t))

;; Sort apropos result by relevance
(setq apropos-sort-by-scores t)
