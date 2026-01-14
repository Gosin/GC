;; Gosin's Emacs Configuration 2026/01/13

;; ------- Basics / Packages -------
(setq inhibit-startup-messages t)

(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(unless package--initialized (package-initialize))
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ------ Server ------
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;; ------- Custom file -------
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; ------ Use UTF-8 Everywhere ------
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; ------- OS detection helpers -------
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-WIN    (eq system-type 'windows-nt))

;; ------- Environment on macOS/*nix (GUI) -------
(use-package exec-path-from-shell
  :if (and (display-graphic-p) (not IS-WIN))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_TTY"))
  :config
  (exec-path-from-shell-initialize))

;; ------- GPG pinentry -------
(use-package pinentry
  :if IS-MAC
  :config
  (pinentry-start))

;; ------- Theme with safe fallback -------
(use-package ef-themes
  :ensure t
  :init
  (setq modus-themes-to-toggle '(ef-dark ef-night))
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t)
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-night :no-confirm))

(setq ef-themes-headings
      '((0 . (bold 1.2))
        (1 . (bold 1.15))
        (2 . (bold 1.10))
        (3 . (bold 1.05))
        (t . (regular 1.0))))

(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

(global-set-key (kbd "C-c t") #'ef-themes-toggle)

;; ------ Window navigation -------
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; ------ Development: Treesitter & LSP ------
;; Auto-install grammars and map modes (Essential for C++/Rust on Emacs 29+)
(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt))

;; Built-in LSP client
(use-package eglot
  :hook ((rust-ts-mode c++-ts-mode python-ts-mode) . eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0))

;; ------ Git Integration (Magit) ------
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  ;; Fix Git path on Windows
  (when IS-WIN
    (let ((git-path (string-trim (shell-command-to-string "where git 2>null"))))
      (when (and (stringp git-path)
                 (file-exists-p git-path))
        (setq magit-git-executable git-path)
        (add-to-list 'exec-path (file-name-directory git-path))))))

;; ------ Paredit / delimiters -------
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-j") nil)
  
  (define-key paredit-mode-map (kbd "M-<right>")     #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-<left>")      #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-S-<right>")   #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-S-<left>")    #'paredit-backward-barf-sexp)
  
  (define-key paredit-mode-map (kbd "C-S-k") #'kill-line)
  
  (define-key paredit-mode-map (kbd "M-d")   #'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "M-DEL") #'paredit-backward-kill-word)
  
  (define-key paredit-mode-map (kbd "M-s") #'isearch-forward-symbol-at-point)
  
  (define-key paredit-mode-map (kbd "C-c (") #'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "C-c [") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "C-c {") #'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "C-c \"") #'paredit-meta-doublequote))

(defun my/scratch-c-j-as-eval-print ()
  (local-set-key (kbd "C-j") #'eval-print-last-sexp))
(add-hook 'lisp-interaction-mode-hook #'my/scratch-c-j-as-eval-print)

(global-set-key (kbd "C-c C-j") #'eval-print-last-sexp)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ------- Org basics -------
(use-package org
  :init
  ;; Logic consolidated here so config doesn't overwrite it
  (setq org-directory (or (bound-and-true-p org-directory)
                          (expand-file-name "Org" (or (getenv "ORG_HOME")
                                                      (expand-file-name "~")))))
  (setq org-default-notes-file (expand-file-name "note.org" org-directory))
  :config
  ;; Create directory if missing
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (require 'org-tempo))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-contrib :after org)

;; ------ Hippie expand -------
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; ------- JS Mode -------
(cond
 ((boundp 'js-ts-mode)
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode)))
 (t
  (use-package js2-mode
    :mode "\\.js\\'")))

;; ------ Lilypond ------
(let* ((cands (list "/opt/homebrew/share/emacs/site-lisp/lilypond"
                    "/usr/local/share/emacs/site-lisp/lilypond"
                    "/usr/share/emacs/site-lisp/lilypond"))
       (lpdir (seq-find #'file-directory-p cands)))
  (when lpdir
    (add-to-list 'load-path lpdir)
    (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
    (add-to-list 'auto-mode-alist '("\\.ily?\\'" . LilyPond-mode))
    (add-hook 'LilyPond-mode-hook #'turn-on-font-lock)))

;; ------ Yasnippet ------
(use-package yasnippet
  :hook ((prog-mode text-mode conf-mode) . yas-minor-mode)
  :config (yas-reload-all))
(use-package yasnippet-snippets :after yasnippet)

;; ------ Load personal library ------
(let ((goer (expand-file-name "goer.el" user-emacs-directory)))
  (when (file-exists-p goer) (load-file goer)))

;; ------ macOS/Windows modifiers ------
(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

(when IS-WIN
  (setq w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super))

;; ------ Enable Emoji Fonts ------
(when IS-WIN
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

(when IS-MAC
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend))

(when IS-LINUX
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend))
