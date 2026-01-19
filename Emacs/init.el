;; Gosin's Emacs Configuration 2026/01/13

;; ============================================================
;; 1. BASICS & PACKAGE MANAGEMENT
;; ============================================================

;; Disable the default startup screen to get to the scratch buffer faster.
(setq inhibit-startup-messages t)

;; Remove the 3D "box" effect from mode-lines for a flatter, modern look.
(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

;; Initialize the package manager.
(require 'package)

;; Add 'melpa' (community packages) and 'nongnu' to the package archives.
;; 'gnu' is the official repository.
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Initialize packages if they haven't been already.
(unless package--initialized (package-initialize))
;; Refresh package contents if the archive definition is empty (first run).
(unless package-archive-contents (package-refresh-contents))

;; Bootstrap 'use-package'.
;; This macro is standard for configuring packages in a tidy, declarative way.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Ensure that 'use-package' will automatically download packages if they
;; are missing. This makes your config portable to new machines.
(setq use-package-always-ensure t)

;; ============================================================
;; 2. CORE UTILITIES
;; ============================================================

;; Start the Emacs server.
;; This allows you to open files in this instance of Emacs from the terminal
;; using the command `emacsclient -nw filename`.
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;; Keep 'init.el' clean.
;; Emacs automatically adds customization settings (e.g., from the GUI menu)
;; to the end of your init file. We redirect those to a separate file.
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Force UTF-8 Encoding.
;; This is critical for cross-platform compatibility (especially Windows),
;; preventing issues with special characters in buffers, terminals, or filenames.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; ============================================================
;; 3. OS SPECIFIC SETUP
;; ============================================================

;; Define constants for cleaner OS checks later in the file.
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-WIN    (eq system-type 'windows-nt))

;; Fix PATH environment variable on macOS/Linux GUI.
;; When launching Emacs from the GUI (Finder/Dock), it doesn't inherit
;; the shell's PATH (e.g., things in .zshrc). This package fixes that.
(use-package exec-path-from-shell
  :if (and (display-graphic-p) (not IS-WIN))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_TTY"))
  :config
  (exec-path-from-shell-initialize))

;; Enable GPG Pinentry support.
;; Required to type GPG passwords (for signing git commits/emails) inside Emacs.
(use-package pinentry
  :if IS-MAC
  :config
  (pinentry-start))

;; ============================================================
;; 4. VISUALS & THEMES
;; ============================================================

;; Ef-themes: High contrast, legible themes by Protesilaos.
(use-package ef-themes
  :ensure t
  :init
  ;; Define which two themes to cycle between with the toggle command.
  (setq modus-themes-to-toggle '(ef-dark ef-night))
  ;; Use variable-pitch (non-monospaced) fonts for UI elements like headings.
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t)
  :config
  ;; Disable any active themes before loading to prevent color bleeding.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-night :no-confirm))

;; Customize heading sizes (Org-mode, Outline-mode) to be larger/bold.
(setq ef-themes-headings
      '((0 . (bold 1.2))   ; Document Title
        (1 . (bold 1.15))  ; Heading 1
        (2 . (bold 1.10))  ; Heading 2
        (3 . (bold 1.05))  ; Heading 3
        (t . (regular 1.0))))

;; Re-apply the "no box" fix in case the theme reset it.
(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

(global-set-key (kbd "C-c t") #'ef-themes-toggle)

;; ============================================================
;; 5. NAVIGATION & DEVELOPMENT
;; ============================================================

;; Window navigation using Shift+Arrow keys.
;; We use the 'meta' modifier (Meta+Shift+Arrows) to avoid conflicts with Org-mode,
;; which uses Shift+Arrows for date manipulation/structure editing.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; --- Tree-sitter (Parsing) ---
;; Automatically install and manage tree-sitter grammars (required for modern
;; syntax highlighting in Emacs 29+).
(use-package treesit-auto
  :config
  ;; Automatically switch to tree-sitter modes (e.g., python-mode -> python-ts-mode)
  (global-treesit-auto-mode)
  ;; Ask before installing a new grammar when a file is opened.
  (setq treesit-auto-install 'prompt))

;; --- LSP (Language Server Protocol) ---
;; add homebrew bin package to exec path
(add-to-list 'exec-path "/opt/homebrew/bin/")
(setenv "PATH" (concat "/opt/homebrew/bin/:" (getenv "PATH")))
;; 'eglot' is the built-in LSP client (Emacs 29+). It connects to external
;; language servers (rust-analyzer, clangd, pyright) for code completion.
(use-package eglot
  :hook ((rust-ts-mode c++-ts-mode python-ts-mode) . eglot-ensure)
  :config
  ;; Optimization: Don't log every single JSON event to a buffer (improves speed).
  (setq eglot-events-buffer-size 0))

;; add envrc
(use-package envrc
  :config
  (envrc-global-mode))

;; Only start Eglot for specific modes
(add-hook 'envrc-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode 'js-mode 'typescript-mode 'rust-mode)
              (eglot-ensure))))

;; --- Git Integration (Magit) ---
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  ;; Windows Specific:
  ;; Emacs on Windows often struggles to find the 'git' executable if it's not
  ;; in the standard path. We use 'where git' to find it and set it explicitly.
  (when IS-WIN
    (let ((git-path (string-trim (shell-command-to-string "where git 2>null"))))
      (when (and (stringp git-path)
                 (file-exists-p git-path))
        (setq magit-git-executable git-path)
        ;; Add git's folder to exec-path so subprocesses work correctly.
        (add-to-list 'exec-path (file-name-directory git-path))))))

;; --- Lisp Editing (Paredit) ---
;; Enforces balanced parentheses. Essential for Lisp/Scheme/Clojure.
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

(with-eval-after-load 'paredit
  ;; Unbind C-j so we can use it for other things (standard paredit uses it for newline).
  (define-key paredit-mode-map (kbd "C-j") nil)
  
  ;; Slurp/Barf bindings:
  ;; "Slurp": Pull the next symbol INTO the current parentheses.
  ;; "Barf": Push the last symbol OUT of the current parentheses.
  (define-key paredit-mode-map (kbd "M-<right>")     #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-<left>")      #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-S-<right>")   #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-S-<left>")    #'paredit-backward-barf-sexp)
  
  ;; Restore standard kill-line behavior on C-S-k (Paredit changes C-k to be structure-safe).
  (define-key paredit-mode-map (kbd "C-S-k") #'kill-line)
  
  ;; Better word killing inside s-expressions.
  (define-key paredit-mode-map (kbd "M-d")   #'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "M-DEL") #'paredit-backward-kill-word)
  
  ;; Restore standard isearch on M-s.
  (define-key paredit-mode-map (kbd "M-s") #'isearch-forward-symbol-at-point)
  
  ;; Quick wrappers: select text and press these to wrap in (), [], {}, or "".
  (define-key paredit-mode-map (kbd "C-c (") #'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "C-c [") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "C-c {") #'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "C-c \"") #'paredit-meta-doublequote))

;; In Scratch buffer only: Map C-j to 'eval-print-last-sexp'.
;; This lets you type (+ 1 1) and hit C-j to see '2' inserted on the next line.
(defun my/scratch-c-j-as-eval-print ()
  (local-set-key (kbd "C-j") #'eval-print-last-sexp))
(add-hook 'lisp-interaction-mode-hook #'my/scratch-c-j-as-eval-print)

;; Global shortcut for eval-print anywhere else.
(global-set-key (kbd "C-c C-j") #'eval-print-last-sexp)

;; Colorize parentheses by depth (helps readability in Lisp).
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================
;; 6. ORG MODE
;; ============================================================
(use-package org
  :init
  ;; Logic: Check if ORG_HOME env var exists, otherwise use "~/Org".
  ;; We do this in :init so the variable is set before Org loads.
  (setq org-directory (or (bound-and-true-p org-directory)
                          (expand-file-name "Org" (or (getenv "ORG_HOME")
                                                      (expand-file-name "~")))))
  (setq org-default-notes-file (expand-file-name "note.org" org-directory))
  :config
  ;; Ensure the org directory actually exists.
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; Save clock history across Emacs restarts.
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Enable template expansion (e.g. typing <s TAB expands to a source block).
  (require 'org-tempo))

;; Make bullet points look nicer (SVG bullets instead of asterisks).
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-contrib :after org)

;; ============================================================
;; 7. MISC LANGUAGES & TOOLS
;; ============================================================

;; Smart Autocomplete: Tries to complete words based on open buffers.
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; JavaScript Configuration:
;; Check if the modern `js-ts-mode` (Tree-sitter) is available.
;; If yes, remap standard js-mode to use it.
;; If no, fall back to `js2-mode` (older but robust package).
(cond
 ((boundp 'js-ts-mode)
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode)))
 (t
  (use-package js2-mode
    :mode "\\.js\\'")))

;; LilyPond Music Notation:
;; Search common install paths to find the Emacs mode files.
(let* ((cands (list "/opt/homebrew/share/emacs/site-lisp/lilypond"
                    "/usr/local/share/emacs/site-lisp/lilypond"
                    "/usr/share/emacs/site-lisp/lilypond"))
       (lpdir (seq-find #'file-directory-p cands)))
  (when lpdir
    (add-to-list 'load-path lpdir)
    (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
    (add-to-list 'auto-mode-alist '("\\.ily?\\'" . LilyPond-mode))
    (add-hook 'LilyPond-mode-hook #'turn-on-font-lock)))

;; Snippets (Yasnippet):
;; Expand templates (like typing 'for' -> tab -> full for loop).
(use-package yasnippet
  :hook ((prog-mode text-mode conf-mode) . yas-minor-mode)
  :config (yas-reload-all))
;; Load a standard library of snippets for many languages.
(use-package yasnippet-snippets :after yasnippet)

;; Load personal helper functions if the file "goer.el" exists.
(let ((goer (expand-file-name "goer.el" user-emacs-directory)))
  (when (file-exists-p goer) (load-file goer)))

;;; Python Development with uv tool, eglot, ruff server, and pyright

;; ============================================================================
;; PREREQUISITES
;; ============================================================================
;; Install tools globally with uv:
;; uv tool install ruff
;; uv tool install pyright

;; ============================================================================
;; BASIC PYTHON SETUP
;; ============================================================================
(setq python-shell-interpreter "uv"
      python-shell-interpreter-args "run python")

;; ============================================================================
;; EGLOT CONFIGURATION
;; ============================================================================
(use-package eglot
  :hook (python-mode . eglot-ensure)
  :config
  
  ;; Configure language servers for Python
  ;; ruff server for linting and formatting
  ;; pyright for type checking
  (add-to-list 'eglot-server-programs
               '(python-mode . ("uvx" "ruff" "server")))
  
  ;; Optional: Use both ruff server and pyright together
  ;; Uncomment if you want both servers running simultaneously
  ;; (add-to-list 'eglot-server-programs
  ;;              '(python-mode . (eglot-alternatives
  ;;                               '(("uvx" "ruff" "server")
  ;;                                 ("uvx" "pyright-langserver" "--stdio")))))
  
  ;; Format on save with ruff
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 
                        'eglot-format-buffer nil 'local))))

;; ============================================================================
;; UV PROJECT MANAGEMENT FUNCTIONS
;; ============================================================================

;; Initialize new uv project
(defun my/uv-init ()
  "Initialize a new uv project"
  (interactive)
  (let ((default-directory (read-directory-name "Project directory: ")))
    (async-shell-command "uv init")))

;; Add dependencies
(defun my/uv-add (package)
  "Add a package using uv"
  (interactive "sPackage name: ")
  (compile (format "uv add %s" package)))

;; Add dev dependencies
(defun my/uv-add-dev (package)
  "Add a dev package using uv"
  (interactive "sPackage name: ")
  (compile (format "uv add --dev %s" package)))

;; Sync dependencies
(defun my/uv-sync ()
  "Sync project dependencies"
  (interactive)
  (compile "uv sync"))

;; Run current buffer
(defun my/uv-run ()
  "Run current file with uv"
  (interactive)
  (compile (format "uv run %s" (buffer-file-name))))

;; ============================================================================
;; RUFF COMMANDS (using uvx for global tool)
;; ============================================================================

;; Check with ruff
(defun my/ruff-check ()
  "Run ruff check on current buffer"
  (interactive)
  (compile (format "uvx ruff check %s" (buffer-file-name))))

;; Fix with ruff
(defun my/ruff-fix ()
  "Fix current file with ruff"
  (interactive)
  (shell-command (format "uvx ruff check --fix %s" (buffer-file-name)))
  (revert-buffer t t t))

;; Format with ruff (manual)
(defun my/ruff-format ()
  "Format current buffer with ruff"
  (interactive)
  (shell-command (format "uvx ruff format %s" (buffer-file-name)))
  (revert-buffer t t t))

;; ============================================================================
;; PYRIGHT TYPE CHECKING (using uvx for global tool)
;; ============================================================================

(defun my/pyright-check ()
  "Run pyright type checking on current file"
  (interactive)
  (compile (format "uvx pyright %s" (buffer-file-name))))

(defun my/pyright-check-project ()
  "Run pyright on entire project"
  (interactive)
  (compile "uvx pyright"))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(add-hook 'python-mode-hook
          (lambda ()
            ;; uv commands
            (local-set-key (kbd "C-c v r") 'my/uv-run)
            (local-set-key (kbd "C-c v a") 'my/uv-add)
            (local-set-key (kbd "C-c v d") 'my/uv-add-dev)
            (local-set-key (kbd "C-c v s") 'my/uv-sync)
            
            ;; ruff commands
            (local-set-key (kbd "C-c r c") 'my/ruff-check)
            (local-set-key (kbd "C-c r f") 'my/ruff-fix)
            (local-set-key (kbd "C-c r m") 'my/ruff-format)
            
            ;; pyright commands
            (local-set-key (kbd "C-c t c") 'my/pyright-check)
            (local-set-key (kbd "C-c t p") 'my/pyright-check-project)
            
            ;; eglot commands (already built-in, but listed for reference)
            ;; C-c l a - code actions
            ;; C-c l r - rename
            ;; C-c l f - format buffer
            ;; C-c l = - format region
            ))

;; ============================================================================
;; OPTIONAL: COMPANY/COMPLETION SETUP
;; ============================================================================

;; If using company-mode for completion
(use-package company
  :hook (python-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; ============================================================
;; 8. KEYBOARD MODIFIERS
;; ============================================================

;; macOS: Map 'Option' to Meta (M) and 'Command' to Super (s).
(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; Windows: Map 'Windows Key' to Super.
(when IS-WIN
  (setq w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super))

;; ============================================================
;; 9. FONTS
;; ============================================================

;; Enable Color Emoji support based on OS.
;; Emacs often defaults to monochrome glyphs unless explicitly told otherwise.
(when IS-WIN
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

(when IS-MAC
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend))

(when IS-LINUX
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend))
