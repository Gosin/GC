;; Gosin's Emacs Configuration 2025/08/23

;; ------- Basics / Packages -------
(setq inhibit-startup-messages t)

(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

(require 'package)
(setq package-archives
      '(("gnu"      . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
        ("melpa"    . "https://melpa.org/packages/")))

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

;; ------ Install package ------
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; ------ Use UTF-8 Everywher ------
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
  (pinentry-start))                     ; On macOS, ensure 'pinentry-mac' is installed; On Windows use Gpg4win

;; ------- Theme with safe fallback -------
;; Ef themes (by Protesilaos)
(use-package ef-themes
  :ensure t
  :init
  ;; Optional: define a quick toggle pair (pick any two you like)
  (setq ef-themes-to-toggle '(ef-dark ef-night))  ;; ef-night is near black (nice on OLED)
  ;; Optional polish
  (setq ef-themes-mixed-fonts t                 ; use variable pitch for prose
        ef-themes-variable-pitch-ui t)          ; variable-pitch UI where appropriate
  :config
  ;; Avoid mixing with any previously enabled theme (prevents odd faces)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-night :no-confirm))

;; Tweak Org/outline heading sizes (example)
(setq ef-themes-headings
      '((0 . (bold 1.2))
        (1 . (bold 1.15))
        (2 . (bold 1.10))
        (3 . (bold 1.05))
        (t . (regular 1.0))))

;; If you previously had boxed modelines, make sure they’re off:
(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

;; Optional: bind a toggle key
(global-set-key (kbd "C-c t") #'ef-themes-toggle)

;; ------ Window navigation -------
;; Use Shift+Meta+Arrows to avoid Org shift-selection conflicts
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; ------ Paredit / delimiters -------
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

;; --- Paredit: sane starter overrides ---
(with-eval-after-load 'paredit
  ;; 1) Newline keys
  (define-key paredit-mode-map (kbd "C-j") nil) ; free C-j
  ;; (Keep RET = paredit-newline; it's great. If you want vanilla RET:
  ;; (define-key paredit-mode-map (kbd "RET") #'newline))

  ;; 2) Slurp / Barf on arrows (ergonomic)
  (define-key paredit-mode-map (kbd "M-<right>")     #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-<left>")      #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-S-<right>")   #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-S-<left>")    #'paredit-backward-barf-sexp)
  ;; If your terminal eats M-S-<arrow>, try these instead:
  ;; (define-key paredit-mode-map (kbd "C-M-f") #'paredit-forward-slurp-sexp)
  ;; (define-key paredit-mode-map (kbd "C-M-b") #'paredit-backward-slurp-sexp)
  ;; (define-key paredit-mode-map (kbd "C-M-]") #'paredit-forward-barf-sexp)
  ;; (define-key paredit-mode-map (kbd "C-M-[") #'paredit-backward-barf-sexp)

  ;; 3) Killing: keep both behaviors handy
  ;; Paredit makes C-k structure-aware; keep a normal kill-line too:
  (define-key paredit-mode-map (kbd "C-S-k") #'kill-line)

  ;; 4) Word deletes (sometimes missed under paredit)
  (define-key paredit-mode-map (kbd "M-d")   #'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "M-DEL") #'paredit-backward-kill-word)

  ;; 5) Restore symbol isearch on M-s (paredit uses M-s for splice-kill-forward)
  (define-key paredit-mode-map (kbd "M-s") #'isearch-forward-symbol-at-point)

  ;; 6) Quick wrappers (handy quality-of-life)
  (define-key paredit-mode-map (kbd "C-c (") #'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "C-c [") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "C-c {") #'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "C-c \"") #'paredit-meta-doublequote))

;; Make C-j do eval-print in *scratch* only (nice for quick experiments)
(defun my/scratch-c-j-as-eval-print ()
  (local-set-key (kbd "C-j") #'eval-print-last-sexp))
(add-hook 'lisp-interaction-mode-hook #'my/scratch-c-j-as-eval-print)

;; Optional: use C-c C-j for eval-print anywhere
(global-set-key (kbd "C-c C-j") #'eval-print-last-sexp)


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ------- Org basics -------
(use-package org
  :init
  (setq org-directory (or (bound-and-true-p org-directory)
                          (expand-file-name "Org" (or (getenv "ORG_HOME")
                                                      (expand-file-name "~")))))
  :config
  ;; Keybindings
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; Clock persistence
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Capture notes default
  (setq org-directory (expand-file-name "~/org"))
  (make-directory org-directory t)
  
  (let ((notes (expand-file-name "note.org" org-directory)))
    (unless (file-exists-p notes)
      (with-temp-file notes))
    (setq org-default-notes-file notes))

  ;; Enable org-tempo for <s TAB etc.
  (require 'org-tempo))

;; Nicer bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-contrib :after org)

;; ------ Hippie expand -------
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; ------- JS Mode: prefer tree-sitter on Emacs 29+, else js2 -------
(cond
 ((boundp 'js-ts-mode)                  ; Emacs 29+
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode)))
 (t
  (use-package js2-mode
    :mode "\\.js\\'")))

;; ------ Lilypond: add whichever path exists
(let* ((cands (list "/opt/homebrew/share/emacs/site-lisp/lilypond" ; macOS M chip
                    "/usr/local/share/emacs/site-lisp/lilypond"   ; macOS Intel / Some Linux
                    "/usr/share/emacs/site-lisp/lilypond"))       ; Linux
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

;; ------ Load personal library (only if present) ------
(let ((goer (expand-file-name "goer.el" user-emacs-directory)))
  (when (file-exists-p goer) (load-file goer)))

;; ------ macOS/Windows optional key modifiers ------
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
;; ------ Notes on terminal keybindings ------
;; For terminal emacs, prefer using GUI where possible for richer key events.
