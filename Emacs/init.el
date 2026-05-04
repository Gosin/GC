;; Gosin's Emacs Configuration 2026/04/29
;; Optimized for Emacs 30, Python uv workflow, Org/Roam, and Structural Lisp Editing

;; ============================================================
;; 1. BASICS & PACKAGE MANAGEMENT
;; ============================================================

;; Disable the default startup screen to get to the scratch buffer faster.
(setq inhibit-startup-messages t)

;; Raise GC threshold during startup for faster loading.
(setq gc-cons-threshold (* 100 1024 1024))

;; Remove the 3D box effect from mode-lines for a flatter look.
(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

(require 'package)

;; Optimization for Native Compilation (Emacs 30)
;; native-comp-available-p is a function, not a variable.
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence the compiler warning buffer to prevent annoying popups.
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Automatically clean up old compilation cache files.
  (setq native-compile-prune-cache t)
  ;; Increase the number of simultaneous compilation jobs (optional, 
  ;; matches half your CPU threads for a good balance).
  (setq native-comp-async-jobs-number 4))

;; Force newer Org from ELPA if desired.
;; These are internal package.el variables, so guard them carefully.
(when (boundp 'package--builtins)
  (assq-delete-all 'org package--builtins))
(when (boundp 'package--builtin-versions)
  (assq-delete-all 'org package--builtin-versions))

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Keep hand-written init.el clean.
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Force UTF-8 encoding.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; ============================================================
;; 2. MACHINE-LOCAL SETTINGS
;; ============================================================

(defvar my/org-babel-languages '((emacs-lisp . t) (shell . t))
  "Babel languages to activate. Override in local.el.")

;; Load machine-local settings early.
(let ((local (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local)
    (load local)))

;; Preserve old ORG_HOME semantics:
;;   org-directory already set in local.el > ORG_HOME/Org > ~/Org
(setq org-directory
      (file-name-as-directory
       (or (bound-and-true-p org-directory)
           (expand-file-name "Org"
                             (or (getenv "ORG_HOME")
                                 (expand-file-name "~"))))))

(setq org-default-notes-file
      (or (bound-and-true-p org-default-notes-file)
          (expand-file-name "note.org" org-directory)))

;; ============================================================
;; 3. OS-SPECIFIC SETUP & CORE UTILITIES
;; ============================================================

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WIN   (eq system-type 'windows-nt))

;; Fix PATH for GUI Emacs on macOS/Linux.
(use-package exec-path-from-shell
  :if (and (display-graphic-p) (not IS-WIN))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_TTY"))
  :config
  (exec-path-from-shell-initialize))

;; Start Emacs server.
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Enable ANSI color in compilation buffers.
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Keep generated files out of init directory.
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; GPG pinentry support.
(use-package pinentry
  :if (and (display-graphic-p) (not IS-WIN))
  :config
  (pinentry-start))

;; ============================================================
;; 4. COMPLETION STACK
;; ============================================================

;; Vertical minibuffer completion UI.
(use-package vertico
  :init
  (vertico-mode)
  :custom
  ;; Allows selecting the prompt itself as a candidate.
  (vertico-preselect 'prompt))

;; Flexible matching.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; Rich annotations in completion UI.
(use-package marginalia
  :init
  (marginalia-mode))

;; Search/navigation commands.
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)))

;; Contextual actions.
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; In-buffer completion popup.
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1))

;; Better fallback expansion.
(global-set-key [remap dabbrev-expand] #'yas-expand)

;; ============================================================
;; 5. VISUALS & THEMES
;; ============================================================

(use-package ef-themes
  :custom
  ;; Using :custom ensures these are set correctly even with the new aliases
  (ef-themes-to-toggle '(ef-night ef-dark))
  (ef-themes-headings
   '((0 . (bold 1.2))
     (1 . (bold 1.15))
     (2 . (bold 1.10))
     (3 . (bold 1.05))
     (t . (regular 1.0))))
  :config
  ;; Avoid theme color bleed.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-night :no-confirm)

  ;; Re-apply no-box mode-line fix after theme load.
  (dolist (f '(mode-line mode-line-inactive header-line))
    (ignore-errors (set-face-attribute f nil :box nil))))

;; Emoji font support.
(when IS-WIN
  (set-fontset-font t 'emoji
                    (font-spec :family "Segoe UI Emoji")
                    nil 'prepend))

(when IS-MAC
  (set-fontset-font t 'emoji
                    (font-spec :family "Apple Color Emoji")
                    nil 'prepend))

(when IS-LINUX
  (set-fontset-font t 'emoji
                    (font-spec :family "Noto Color Emoji")
                    nil 'prepend))

;; ============================================================
;; 6. NAVIGATION & DEVELOPMENT CORE
;; ============================================================

;; Set up kbd for windmove. Considering using ace-window in the future
(global-set-key (kbd "C-x w <left>")  'windmove-left)
(global-set-key (kbd "C-x w <right>") 'windmove-right)
(global-set-key (kbd "C-x w <up>")    'windmove-up)
(global-set-key (kbd "C-x w <down>")  'windmove-down)

;; Tree-sitter grammar management.
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Add remaps safely instead of overwriting major-mode-remap-alist.
(dolist (remap '((python-mode     . python-ts-mode)
                 (c++-mode        . c++-ts-mode)
                 (rust-mode       . rust-ts-mode)
                 (javascript-mode . js-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))

;; Per-project environment integration.
(use-package envrc
  :hook (prog-mode . envrc-mode))

;; LSP via Eglot.
(use-package eglot
  :config
  ;; Disable verbose JSON-RPC event logging.
  (setq eglot-events-buffer-size 0)

  ;; Prefer Pyright for both classic and tree-sitter Python modes.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("pyright-langserver" "--stdio"))))

;; Start Eglot only after envrc has had a chance to activate the project env.
(add-hook 'envrc-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-base-mode
                                  'c++-mode 'c++-ts-mode
                                  'rust-mode 'rust-ts-mode
                                  'js-mode 'js-ts-mode
                                  'typescript-mode 'typescript-ts-mode)
              (eglot-ensure))))

;; Git integration.
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  ;; Windows: locate git.exe robustly.
  (when IS-WIN
    (let ((git-path (car (split-string
                          (string-trim
                           (shell-command-to-string "where git 2>null"))
                          "\n"
                          t))))
      (when (and (stringp git-path)
                 (file-exists-p git-path))
        (setq magit-git-executable git-path)
        (add-to-list 'exec-path (file-name-directory git-path))))))

;; ============================================================
;; 7. LISP EDITING
;; ============================================================

(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode)
         . enable-paredit-mode))

(with-eval-after-load 'paredit
  ;; Free C-j for scratch-buffer eval-print binding below.
  (define-key paredit-mode-map (kbd "C-j") nil)

  ;; Slurp/barf structural editing.
  (define-key paredit-mode-map (kbd "M-<right>")
              #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-<left>")
              #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-S-<right>")
              #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-S-<left>")
              #'paredit-backward-barf-sexp)

  ;; Restore useful old bindings.
  (define-key paredit-mode-map (kbd "C-S-k") #'kill-line)
  (define-key paredit-mode-map (kbd "M-d")   #'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "M-DEL") #'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "M-s")   #'isearch-forward-symbol-at-point)

  ;; Wrapping helpers.
  (define-key paredit-mode-map (kbd "C-c (")  #'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "C-c [")  #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "C-c {")  #'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "C-c \"") #'paredit-meta-doublequote))

;; In scratch buffer, use C-j for eval-print-last-sexp.
(defun my/scratch-c-j-as-eval-print ()
  "Bind C-j to `eval-print-last-sexp' in the current buffer."
  (local-set-key (kbd "C-j") #'eval-print-last-sexp))

(add-hook 'lisp-interaction-mode-hook #'my/scratch-c-j-as-eval-print)

;; Global eval-print fallback.
(global-set-key (kbd "C-c C-j") #'eval-print-last-sexp)

;; Colorize nested delimiters.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================
;; 8. SNIPPETS, WEB, HTML, CSS, JS
;; ============================================================

;; Snippets.
(use-package yasnippet
  :hook ((prog-mode text-mode conf-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package apheleia
  :ensure t)

;; Web mode for HTML/HTMX.
(use-package web-mode
  :ensure nil
  :mode ("\\.html\\'" "\\.htmx\\'")
  :hook (web-mode . apheleia-mode)
  :config
  ;; HTML indent
  (setq web-mode-markup-indent-offset 2)
  ;; CSS in HTML indent
  (setq web-mode-css-indent-offset 2)
  ;; JS in HTML indent
  (setq web-mode-code-indent-offset 2)
  ;; <div> -> </div> automatically
  (setq web-mode-enable-auto-closing t)
  ;; class= -> class="" automatically
  (setq web-mode-enable-auto-quoting t))

(use-package css-ts-mode
  :ensure nil
  :mode "\\.css\\'"
  :hook (css-ts-mode . apheleia-mode)
  :config
  (setq css-indent-offset 2))

(use-package js-ts-mode
  :ensure nil
  :mode "\\.js\\'"
  :hook (js-ts-mode . apheleia-mode)
  :config
  (setq js-indent-level 2))

;; Emmet expansion for HTML/CSS.
(use-package emmet-mode
  :hook ((web-mode css-mode css-ts-mode html-ts-mode) . emmet-mode)
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)
              (local-set-key (kbd "C-j") #'emmet-expand-line)))

  (add-hook 'css-ts-mode-hook
            (lambda ()
              (setq-local emmet-use-css-transform t))))

;; Disable electric-indent in markup modes where it tends to fight Emmet.
(dolist (hook '(html-ts-mode-hook))
  (add-hook hook
            (lambda ()
              (electric-indent-local-mode -1))))

;; Fallback JavaScript mode for older Emacs if tree-sitter mode is unavailable.
(unless (fboundp 'js-ts-mode)
  (use-package js2-mode
    :mode "\\.js\\'"))

;; LilyPond Music Notation.
(let* ((cands (list "/opt/homebrew/share/emacs/site-lisp/lilypond"
                    "/usr/local/share/emacs/site-lisp/lilypond"
                    "/usr/share/emacs/site-lisp/lilypond"))
       (lpdir (seq-find #'file-directory-p cands)))
  (when lpdir
    (add-to-list 'load-path lpdir)
    (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
    (add-to-list 'auto-mode-alist '("\\.ily?\\'" . LilyPond-mode))
    (add-hook 'LilyPond-mode-hook #'turn-on-font-lock)))

;; ============================================================
;; 9. ORG MODE & ORG-ROAM
;; ============================================================

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  ;; Ensure Org directory exists on fresh machines.
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))

  ;; Persist running clock across Emacs restarts.
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Load Babel languages from local.el or fallback defvar.
  (org-babel-do-load-languages
   'org-babel-load-languages
   my/org-babel-languages)

  ;; Restore <s TAB source-block expansion.
  (require 'org-tempo))

;; Org-roam.
(use-package org-roam
  :custom
  (org-roam-directory
   (expand-file-name "roam" org-directory))

  :bind (("C-c z l" . org-roam-buffer-toggle)
         ("C-c z f" . org-roam-node-find)
         ("C-c z g" . org-roam-graph)
         ("C-c z i" . org-roam-node-insert)
         ("C-c z d" . org-id-get-create)
         ("C-c z c" . org-roam-capture)
         ("C-c z j" . org-roam-dailies-capture-today))
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))

  (org-roam-db-autosync-mode))

;; Pretty Org bullets.
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; Extra Org modules.
(use-package org-contrib
  :after org)

;; ============================================================
;; 10. PYTHON, UV, RUFF, PYRIGHT
;; ============================================================

;; Force Python's built-in REPL instead of IPython/ptpython-like behavior.
(setenv "PYTHON_BASIC_REPL" "1")

(defvar my/ruff-program "ruff"
  "Program used for Ruff formatting. Usually project-local via envrc or global via uv tool install.")

;; ---------- uv helpers ----------

(defun my/uv-init ()
  "Initialize a new uv project."
  (interactive)
  (let ((default-directory (read-directory-name "Project directory: ")))
    (async-shell-command "uv init")))

(defun my/uv-sync ()
  "Sync project dependencies to match uv.lock."
  (interactive)
  (compile "uv sync"))

(defun my/uv-run ()
  "Run the current buffer's file with uv run."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (compile
   (format "uv run %s"
           (shell-quote-argument buffer-file-name))))

(defun my/uv-add (package)
  "Add a runtime package using uv."
  (interactive "sPackage name: ")
  (compile
   (format "uv add %s"
           (shell-quote-argument package))))

(defun my/uv-add-dev (package)
  "Add a dev-only package using uv."
  (interactive "sPackage name: ")
  (compile
   (format "uv add --dev %s"
           (shell-quote-argument package))))

;; ---------- Ruff helpers ----------

(defun my/ruff-format-buffer ()
  "Format current Python buffer with Ruff via stdin.

This avoids writing the file first, preserves point/window position,
and reports Ruff errors without destroying the current buffer."
  (interactive)
  (unless (derived-mode-p 'python-base-mode)
    (user-error "Not in a Python buffer"))

  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))

  (unless (executable-find my/ruff-program)
    (user-error "Cannot find ruff. Install it with `uv tool install ruff` or make it available in the project environment"))

  (let* ((old-point (point))
         (old-win-start (window-start))
         (tmpbuf (generate-new-buffer " *ruff-output*"))
         (exit-code
          (call-process-region
           (point-min)
           (point-max)
           my/ruff-program
           nil
           tmpbuf
           nil
           "format"
           "--stdin-filename"
           buffer-file-name
           "-")))
    (unwind-protect
        (if (zerop exit-code)
            (progn
              (replace-buffer-contents tmpbuf)
              (goto-char (min old-point (point-max)))
              (set-window-start
               (selected-window)
               (min old-win-start (point-max))))
          (message "ruff format failed with exit %d: %s"
                   exit-code
                   (with-current-buffer tmpbuf
                     (string-trim (buffer-string)))))
      (kill-buffer tmpbuf))))

(defun my/ruff-check ()
  "Run Ruff lint check on the current file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (compile
   (format "uvx ruff check %s"
           (shell-quote-argument buffer-file-name))))

(defun my/ruff-fix ()
  "Auto-fix Ruff lint issues in the current file and reload."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (shell-command
   (format "uvx ruff check --fix %s"
           (shell-quote-argument buffer-file-name)))
  (revert-buffer t t t))

;; ---------- Pyright helpers ----------

(defun my/pyright-check ()
  "Run Pyright type checking on the current file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (compile
   (format "uvx pyright %s"
           (shell-quote-argument buffer-file-name))))

(defun my/pyright-check-project ()
  "Run Pyright type checking on the current project."
  (interactive)
  (compile "uvx pyright"))

;; Ruff diagnostics through Flymake.
(use-package flymake-ruff
  :hook (python-base-mode . flymake-ruff-load))

;; Python keybindings and format-on-save.
(with-eval-after-load 'python
  ;; uv project management
  (define-key python-base-mode-map (kbd "C-c v i") #'my/uv-init)
  (define-key python-base-mode-map (kbd "C-c v s") #'my/uv-sync)
  (define-key python-base-mode-map (kbd "C-c v r") #'my/uv-run)
  (define-key python-base-mode-map (kbd "C-c v a") #'my/uv-add)
  (define-key python-base-mode-map (kbd "C-c v d") #'my/uv-add-dev)

  ;; Ruff lint/format
  (define-key python-base-mode-map (kbd "C-c r c") #'my/ruff-check)
  (define-key python-base-mode-map (kbd "C-c r f") #'my/ruff-fix)
  (define-key python-base-mode-map (kbd "C-c r m") #'my/ruff-format-buffer)

  ;; Pyright type checking
  (define-key python-base-mode-map (kbd "C-c t c") #'my/pyright-check)
  (define-key python-base-mode-map (kbd "C-c t p") #'my/pyright-check-project)

  ;; Conservative behavior: only Python auto-formats on save.
  ;; Other languages are not auto-formatted unless explicitly configured later.
  (add-hook 'python-base-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'my/ruff-format-buffer
                        nil
                        t))))

;; ============================================================
;; 11. KEYBOARD MODIFIERS
;; ============================================================

;; macOS: Option -> Meta, Command -> Super.
(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; Windows: Windows key -> Super.
(when IS-WIN
  (setq w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super))

;; ============================================================
;; 12. PERSONAL HELPERS
;; ============================================================

;; Load personal helper functions if goer.el exists.
(let ((goer (expand-file-name "goer.el" user-emacs-directory)))
  (when (file-exists-p goer)
    (load goer)))

;; Restore lower GC threshold after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; Sort package-selected-packages for easy comparison
(define-advice customize-save-variable
    (:before (variable value &optional comment) sort-packages)
  "Alphabetically sort `package-selected-packages` whenever it is saved via customize."
  (when (eq variable 'package-selected-packages)
    (setq value (sort value #'string<))))
