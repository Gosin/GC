;; Gosin's Emacs Configuration 2023/04/01

;; start emacs as a server
(server-start)

;; Set up pin entry for signing commits (See EasyPG Assistant for more information)
;; Start the server to make it working
(pinentry-start)

;; Import shell environment to Emacs shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Utilize use-package to make sure pacakges are installed
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

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
;; TODO: Adding this hook prevents expression from evaluating
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
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

;; Use js2-mode for javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;; Config lilypond (Install lilypond from homebrew first)
;; Add lilypond directory to load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/lilypond")
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; Load yasnippets, enable yas-minor-mode for other modes to use them.
(require 'yasnippet)
(yas-reload-all)

;; Load personal library
(load-file "~/.emacs.d/goer.el")

;; remap keys for iTerm 2
;; use cat -v or ctrl-v to show the sequence in iTerm 2.
;; ^ is Ctrl, ^[ is Alt/Meta
;; replace ^[ with \e
;; Terminal-based Emacs didn't handle key bindings properly, use GUI-based Emacs onwards
