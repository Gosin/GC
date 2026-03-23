;; Gosin's Emacs Configuration 2026/03/22

;; ============================================================
;; 1. BASICS & PACKAGE MANAGEMENT
;; ============================================================

;; Disable the default startup screen to get to the scratch buffer faster.
(setq inhibit-startup-messages t)

;; Remove the 3D "box" effect from mode-lines for a flatter, modern look.
(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

;; Remove built-in org so that latest org can be installed.
;; Emacs ships with an older version of Org baked in. If we don't remove it
;; from the package registry first, package.el may resolve `org` to the
;; built-in and skip installing the newer MELPA version entirely.
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

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
;; Emacs automatically appends customization settings (e.g., from M-x customize
;; or package install confirmations) to the end of your init file. We redirect
;; those writes to a separate file so your hand-written init.el stays pristine
;; and diffs stay readable in version control.
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

;; Enable Vertico for vertical completion UI.
;; The default completing-read shows candidates in the echo area (hard to scan).
;; Vertico renders them as a vertical list in the minibuffer, which is far easier
;; to navigate with C-n/C-p and works with all standard Emacs completion commands.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Enable Orderless for fuzzy matching.
;; By default Emacs completion requires a prefix match (you must type the start
;; of the candidate). Orderless lets you type space-separated components in any
;; order — e.g. "proj sol" matches "soloist-project". The 'basic' fallback is
;; needed for file completion (TRAMP, etc.) where prefix matching is required.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ============================================================
;; 2.5 MACHINE-LOCAL OVERRIDES
;; ============================================================

;; Fallback babel languages in case local.el is absent (e.g. fresh machine).
;; local.el should override this defvar with the machine-specific language list.
;; We declare it here so that the org :config block can safely reference it
;; even when no local.el exists.
(defvar my/org-babel-languages '((emacs-lisp . t) (shell . t))
  "Babel languages to activate. Override in local.el.")

;; Load machine-specific settings before any package configuration.
;; This ensures values like org-directory, fonts, and babel languages
;; are set before the packages that consume them (org, eglot, etc.) load.
;; local.el is not committed to version control — each machine has its own.
(let ((local (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local)
    (load local)))

;; ============================================================
;; 3. OS SPECIFIC SETUP
;; ============================================================

;; Define constants for cleaner OS checks later in the file.
(defconst IS-MAC    (eq system-type 'darwin))
(defconst IS-LINUX  (eq system-type 'gnu/linux))
(defconst IS-WIN    (eq system-type 'windows-nt))

;; Fix PATH environment variable on macOS/Linux GUI.
;; When launching Emacs from the GUI (Finder/Dock), it doesn't inherit
;; the shell's PATH (e.g., things in .zshrc). This matters because eglot needs
;; to find pyright/ruff, magit needs git, and uv functions need uv — all of which
;; are typically installed into PATH locations that GUI Emacs never sees.
(use-package exec-path-from-shell
  :if (and (display-graphic-p) (not IS-WIN))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GPG_TTY"))
  :config
  (exec-path-from-shell-initialize))

;; Enable GPG Pinentry support.
;; Required to type GPG passwords (for signing git commits/emails) inside Emacs.
;; Without this, gpg-agent tries to pop up a GUI dialog which often fails in
;; terminal Emacs or when the agent was started with loopback pinentry disabled.
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
  ;; Loading a second theme on top of an existing one merges their faces,
  ;; which causes stray colors from the old theme to bleed through.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-night :no-confirm))

;; Customize heading sizes (Org-mode, Outline-mode) to be larger/bold.
;; Must be set before ef-themes loads or calls (ef-themes-select ...).
;; Setting it after load-theme has no effect until the theme is re-applied.
(setq ef-themes-headings
      '((0 . (bold 1.2))   ; Document Title
        (1 . (bold 1.15))  ; Heading 1
        (2 . (bold 1.10))  ; Heading 2
        (3 . (bold 1.05))  ; Heading 3
        (t . (regular 1.0))))

;; Re-apply the "no box" fix in case the theme reset it.
;; Some themes explicitly re-set mode-line faces including :box, so we apply
;; the fix after the theme loads too.
(dolist (f '(mode-line mode-line-inactive header-line))
  (ignore-errors (set-face-attribute f nil :box nil)))

;; FIX: was C-c t, which consumed the prefix and blocked the Python
;; C-c t c / C-c t p pyright bindings defined in section 7.
(global-set-key (kbd "C-c T") #'ef-themes-toggle)

;; ============================================================
;; 5. NAVIGATION & DEVELOPMENT
;; ============================================================

;; Window navigation using Shift+Arrow keys.
;; We use the 'meta' modifier (Meta+Shift+Arrows) to avoid conflicts with Org-mode,
;; which uses Shift+Arrows for date manipulation and outline structure editing.
;; Plain windmove-default-keybindings uses bare Shift+Arrows which Org intercepts first.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; --- Tree-sitter (Parsing) ---
;; Automatically install and manage tree-sitter grammars (required for modern
;; syntax highlighting in Emacs 29+).
;; Tree-sitter grammars are compiled C libraries. treesit-auto detects when a
;; grammar is missing and either installs it automatically or prompts you,
;; so you don't have to run M-x treesit-install-language-grammar manually
;; on each new machine.
(use-package treesit-auto
  :config
  ;; Automatically switch to tree-sitter modes (e.g., python-mode -> python-ts-mode)
  (global-treesit-auto-mode)
  ;; Ask before installing a new grammar when a file is opened.
  (setq treesit-auto-install 'prompt))

;; --- direnv / per-project environments ---
;; envrc integrates Emacs with direnv, which reads .envrc files in project roots.
;; This is critical for uv-based Python projects: each project has its own
;; .venv, and direnv activates it so that pyright, ruff, and python all resolve
;; to the project-local versions rather than whatever is globally on PATH.
;; Without this, eglot may start pyright from the wrong interpreter/environment.
(use-package envrc
  :config
  (envrc-global-mode)
  :hook (after-init . envrc-global-mode))

;; --- LSP (Language Server Protocol) ---
;; Single unified eglot block covering all languages.
;; Previously split across sections 5 and 7, which caused the :config blocks
;; to run in separate use-package expansions, making hook and server-program
;; setup order unpredictable. Merged here so all eglot configuration is
;; in one place and evaluated together.
(use-package eglot
  :hook ((rust-ts-mode c++-ts-mode python-ts-mode python-mode) . eglot-ensure)
  :config
  ;; Optimization: Don't log every single JSON-RPC event to a buffer.
  ;; By default eglot logs the full LSP wire protocol, which for pyright can
  ;; generate thousands of messages per second on large projects, noticeably
  ;; slowing Emacs. Set to 0 to disable the log entirely.
  (setq eglot-events-buffer-size 0)

  ;; Register pyright as the LSP server for Python.
  ;; eglot's default for python-mode is pylsp (Python Language Server), which
  ;; is heavier and less accurate for type inference than pyright. We override
  ;; it here to use pyright-langserver, which is Pylance's open-source core.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))

  ;; Use flymake with ruff for linting diagnostics.
  ;; eglot uses flymake (not flycheck) as its diagnostic backend. flymake-ruff
  ;; adds ruff as a flymake checker so you get ruff lint errors inline in the
  ;; buffer alongside pyright type errors — without running a separate process.
  (use-package flymake-ruff
    :hook (python-mode . flymake-ruff-load))

  ;; Auto-format on save via eglot (which delegates to pyright/ruff).
  ;; eglot-format-buffer sends a textDocument/formatting LSP request, so the
  ;; formatter runs through the same server that provides completion — no
  ;; extra processes. The nil 'local args mean the hook is buffer-local so
  ;; it only fires in Python buffers, not globally on every before-save-hook.
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        'eglot-format-buffer nil 'local))))

;; Start Eglot when envrc activates a new environment.
;; envrc-mode-hook fires after direnv has set up the buffer's environment,
;; so by the time eglot-ensure runs, exec-path already points to the
;; project-local pyright/ruff inside the .venv. Without this hook, eglot
;; started from the normal python-mode hook might launch before direnv
;; has injected the correct PATH.
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
  ;; in the standard PATH. Using 'where git' lets Windows locate it the same
  ;; way a cmd.exe session would, then we pin magit to that exact path so
  ;; subprocesses (git log, git diff, etc.) all use the same binary.
  (when IS-WIN
    (let ((git-path (string-trim (shell-command-to-string "where git 2>null"))))
      (when (and (stringp git-path)
                 (file-exists-p git-path))
        (setq magit-git-executable git-path)
        ;; Add git's folder to exec-path so subprocesses work correctly.
        (add-to-list 'exec-path (file-name-directory git-path))))))

;; --- Lisp Editing (Paredit) ---
;; Enforces balanced parentheses. Essential for Lisp/Scheme/Clojure.
;; Paredit prevents you from accidentally deleting an unmatched paren, which
;; would make the entire file unparseable. It also provides structural
;; movement commands (slurp/barf) that operate on s-expressions as units.
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

(with-eval-after-load 'paredit
  ;; Unbind C-j in paredit so it doesn't shadow our global eval-print binding.
  ;; Paredit uses C-j for newline-and-indent, but we want C-j free for
  ;; eval-print-last-sexp in the scratch buffer (set below).
  (define-key paredit-mode-map (kbd "C-j") nil)

  ;; Slurp/Barf bindings:
  ;; "Slurp": Pull the next sibling expression INTO the current sexp.
  ;; "Barf": Push the last child expression OUT of the current sexp.
  ;; Example: (foo |bar) baz  --slurp-->  (foo |bar baz)
  (define-key paredit-mode-map (kbd "M-<right>")     #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-<left>")      #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-S-<right>")   #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-S-<left>")    #'paredit-backward-barf-sexp)

  ;; Restore standard kill-line on C-S-k.
  ;; Paredit remaps C-k to kill the entire s-expression (structure-aware).
  ;; C-S-k gives back the raw line-kill for the rare case you need it.
  (define-key paredit-mode-map (kbd "C-S-k") #'kill-line)

  ;; Better word killing inside s-expressions.
  ;; Paredit's versions of M-d and M-DEL are structure-aware: they won't
  ;; delete a word if doing so would create an unbalanced paren.
  (define-key paredit-mode-map (kbd "M-d")   #'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "M-DEL") #'paredit-backward-kill-word)

  ;; Restore standard isearch on M-s.
  ;; Paredit binds M-s to paredit-splice-sexp (removes surrounding parens).
  ;; We reassign it to the more commonly needed symbol search; use
  ;; M-x paredit-splice-sexp if you need splicing.
  (define-key paredit-mode-map (kbd "M-s") #'isearch-forward-symbol-at-point)

  ;; Quick wrappers: select a region and press these to wrap it in delimiters.
  ;; Useful when refactoring: select an expression and press C-c ( to wrap it
  ;; in a function call without breaking paren balance.
  (define-key paredit-mode-map (kbd "C-c (") #'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "C-c [") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "C-c {") #'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "C-c \"") #'paredit-meta-doublequote))

;; In the *scratch* buffer only: map C-j to eval-print-last-sexp.
;; The scratch buffer uses lisp-interaction-mode, where C-j is the idiomatic
;; way to evaluate an expression and insert the result inline — handy for
;; quick Elisp experiments without switching to the minibuffer.
(defun my/scratch-c-j-as-eval-print ()
  (local-set-key (kbd "C-j") #'eval-print-last-sexp))
(add-hook 'lisp-interaction-mode-hook #'my/scratch-c-j-as-eval-print)

;; Global shortcut for eval-print anywhere else.
(global-set-key (kbd "C-c C-j") #'eval-print-last-sexp)

;; Colorize parentheses by depth.
;; In deeply nested Lisp code, matching the opening paren of a closing paren
;; visually is painful. Rainbow-delimiters assigns a distinct color to each
;; nesting level, making structure immediately visible.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================
;; 6. ORG MODE
;; ============================================================
(use-package org
  :init
  ;; Resolve org-directory before Org loads.
  ;; Priority: an already-bound org-directory (set in local.el) > ORG_HOME
  ;; environment variable > ~/Org fallback.
  (setq org-directory
        (or (bound-and-true-p org-directory)
            (expand-file-name "Org" (or (getenv "ORG_HOME")
                                        (expand-file-name "~")))))

  ;; FIX: guard org-default-notes-file the same way as org-directory.
  ;; Previously this was set unconditionally, silently overwriting whatever
  ;; local.el had already set for this machine.
  (setq org-default-notes-file
        (or (bound-and-true-p org-default-notes-file)
            (expand-file-name "note.org" org-directory)))
  :config
  ;; Ensure the org directory actually exists on first run on a new machine.
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))

  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; Persist the running clock across Emacs restarts.
  ;; Without this, if you close Emacs while a clock is running (e.g. mid-task),
  ;; the clock entry is silently lost. With persistence, Emacs offers to
  ;; resume or close the clock on next startup.
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Load babel languages declared in local.el (or the fallback defvar above).
  ;; Called here in :config because org must be loaded before
  ;; org-babel-do-load-languages can run — local.el only declares the list.
  (org-babel-do-load-languages 'org-babel-load-languages my/org-babel-languages)

  ;; Enable template expansion (e.g. typing <s TAB expands to a source block).
  ;; org-tempo was split into its own module in Org 9.2. Without requiring it
  ;; explicitly, the <s TAB shorthand silently stops working.
  (require 'org-tempo))

;; Make bullet points look nicer (SVG bullets instead of asterisks).
;; org-superstar replaces the leading * characters with Unicode bullet glyphs,
;; making the visual hierarchy cleaner without changing the underlying file.
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; org-contrib provides extra Org modules not yet in the core distribution,
;; such as org-checklist, org-expiry, and ox-extra. Loaded after org so that
;; org's autoloads are already in place.
(use-package org-contrib :after org)

;; ============================================================
;; 7. MISC LANGUAGES & TOOLS
;; ============================================================

;; Smart Autocomplete: complete words from all open buffers.
;; dabbrev-expand only searches the current buffer. hippie-expand tries a
;; sequence of strategies: buffer words, other-buffer words, file names,
;; yasnippet expansions — making it a much more useful tab-completion fallback
;; when no LSP is active.
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; JavaScript Configuration.
;; js-ts-mode (Emacs 29+, tree-sitter based) gives better syntax highlighting
;; and faster parsing than the older js-mode. We check at runtime so the config
;; still works on Emacs 28 by falling back to js2-mode.
(cond
 ((boundp 'js-ts-mode)
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode)))
 (t
  (use-package js2-mode
    :mode "\\.js\\'")))

;; LilyPond Music Notation.
;; LilyPond installs its Emacs mode files to a non-standard location that
;; isn't on load-path by default. We probe the common install paths so the
;; mode works regardless of whether LilyPond was installed via Homebrew,
;; a Linux package manager, or manually — without hardcoding a machine-specific
;; path in the shared config.
(let* ((cands (list "/opt/homebrew/share/emacs/site-lisp/lilypond"
                    "/usr/local/share/emacs/site-lisp/lilypond"
                    "/usr/share/emacs/site-lisp/lilypond"))
       (lpdir (seq-find #'file-directory-p cands)))
  (when lpdir
    (add-to-list 'load-path lpdir)
    (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
    (add-to-list 'auto-mode-alist '("\\.ily?\\'" . LilyPond-mode))
    (add-hook 'LilyPond-mode-hook #'turn-on-font-lock)))

;; Snippets (Yasnippet).
;; Yasnippet expands short triggers (e.g. 'def' TAB) into full code templates.
;; We enable yas-minor-mode in prog, text, and conf modes so snippets work
;; everywhere you write code or structured text, but not in special buffers
;; (dired, magit, etc.) where TAB has other meanings.
(use-package yasnippet
  :hook ((prog-mode text-mode conf-mode) . yas-minor-mode)
  :config (yas-reload-all))
;; Load a standard library of community snippets for many languages.
;; Provides a useful baseline so you don't have to author snippets from scratch.
(use-package yasnippet-snippets :after yasnippet)

;; Load personal helper functions if the file "goer.el" exists.
;; Keeping personal utilities in a separate file avoids clutter here and
;; lets you share init.el without exposing private helpers.
(let ((goer (expand-file-name "goer.el" user-emacs-directory)))
  (when (file-exists-p goer) (load-file goer)))

;;; Python Development with uv, eglot, ruff server, and pyright

;; ============================================================================
;; PREREQUISITES
;; ============================================================================
;; Install tools globally with uv:
;;   uv tool install ruff
;;   uv tool install pyright
;;
;; These are installed as global uv tools (not per-project), so they're
;; available even before a project's .venv is activated. Per-project versions
;; (if present in the venv) are preferred automatically because envrc puts
;; .venv/bin first on PATH.

;; ============================================================================
;; BASIC PYTHON SETUP
;; ============================================================================

;; Force Python's built-in REPL instead of IPython/ptpython.
;; Some tools detect an enhanced REPL and alter their output format, which
;; can confuse Emacs's comint-based python-mode shell. This env var opts out.
(setenv "PYTHON_BASIC_REPL" "1")

;; ============================================================================
;; UV PROJECT MANAGEMENT FUNCTIONS
;; ============================================================================

;; Initialize new uv project.
;; async-shell-command is used (not compile) because `uv init` is a one-shot
;; scaffolding command — we don't need to re-run it or parse its output for
;; errors; we just want it to complete in the background.
(defun my/uv-init ()
  "Initialize a new uv project."
  (interactive)
  (let ((default-directory (read-directory-name "Project directory: ")))
    (async-shell-command "uv init")))

;; Add a runtime dependency.
;; compile is used here (vs shell-command) so the output appears in a
;; *compilation* buffer with error highlighting, and you can re-run with `g`.
(defun my/uv-add (package)
  "Add a runtime package using uv."
  (interactive "sPackage name: ")
  (compile (format "uv add %s" package)))

;; Add a dev-only dependency (not included in production installs).
(defun my/uv-add-dev (package)
  "Add a dev-only package using uv (excluded from production installs)."
  (interactive "sPackage name: ")
  (compile (format "uv add --dev %s" package)))

;; Sync the virtual environment to match pyproject.toml / uv.lock.
;; Run this after pulling changes from git to ensure your local .venv
;; matches what the lockfile specifies.
(defun my/uv-sync ()
  "Sync project dependencies to match uv.lock."
  (interactive)
  (compile "uv sync"))

;; Run the current file through uv run, which activates the project venv
;; automatically. Avoids having to manually activate the venv in a terminal.
(defun my/uv-run ()
  "Run the current buffer's file with uv run."
  (interactive)
  (compile (format "uv run %s" (buffer-file-name))))

;; ============================================================================
;; RUFF COMMANDS
;; ============================================================================
;; We use `uvx ruff` rather than a bare `ruff` call so these commands work
;; even on machines where ruff isn't on PATH — uvx downloads and runs the
;; tool in an isolated environment on demand.

(defun my/ruff-check ()
  "Run ruff lint check on the current buffer."
  (interactive)
  (compile (format "uvx ruff check %s" (buffer-file-name))))

;; ruff --fix applies safe auto-fixes (unused imports, etc.) in place.
;; We revert the buffer after so Emacs reflects the on-disk changes immediately.
(defun my/ruff-fix ()
  "Auto-fix lint issues in the current buffer with ruff."
  (interactive)
  (shell-command (format "uvx ruff check --fix %s" (buffer-file-name)))
  (revert-buffer t t t))

;; ruff format is ruff's Black-compatible formatter (separate from linting).
(defun my/ruff-format ()
  "Format the current buffer with ruff."
  (interactive)
  (shell-command (format "uvx ruff format %s" (buffer-file-name)))
  (revert-buffer t t t))

;; ============================================================================
;; PYRIGHT TYPE CHECKING
;; ============================================================================

;; Run pyright on the current file only — faster feedback during development.
(defun my/pyright-check ()
  "Run pyright type checking on the current file."
  (interactive)
  (compile (format "uvx pyright %s" (buffer-file-name))))

;; Run pyright on the entire project — useful before committing or in CI review.
(defun my/pyright-check-project ()
  "Run pyright type checking on the entire project."
  (interactive)
  (compile "uvx pyright"))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================
;; All Python-specific bindings are scoped to python-mode-hook so they don't
;; pollute the global keymap or conflict with other major modes.

(add-hook 'python-mode-hook
          (lambda ()
            ;; uv project management
            (local-set-key (kbd "C-c v r") 'my/uv-run)
            (local-set-key (kbd "C-c v a") 'my/uv-add)
            (local-set-key (kbd "C-c v d") 'my/uv-add-dev)
            (local-set-key (kbd "C-c v s") 'my/uv-sync)

            ;; ruff lint/format
            (local-set-key (kbd "C-c r c") 'my/ruff-check)
            (local-set-key (kbd "C-c r f") 'my/ruff-fix)
            (local-set-key (kbd "C-c r m") 'my/ruff-format)

            ;; pyright type checking
            ;; Note: C-c t prefix is free because theme toggle was moved to C-c T
            (local-set-key (kbd "C-c t c") 'my/pyright-check)
            (local-set-key (kbd "C-c t p") 'my/pyright-check-project)

            ;; eglot built-in bindings (listed for reference):
            ;; C-c l a  - code actions (quick fixes, extract variable, etc.)
            ;; C-c l r  - rename symbol across project
            ;; C-c l f  - format buffer (via LSP)
            ;; C-c l =  - format region
            ))

;; ============================================================================
;; COMPLETION (company-mode)
;; ============================================================================
;; company-mode provides an inline popup for LSP completions from eglot.
;; Without it, completions are available but only via M-TAB / C-M-i (less fluid).
;; minimum-prefix-length 1 and idle-delay 0.0 make it feel instant.
(use-package company
  :hook (python-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; ============================================================
;; 8. KEYBOARD MODIFIERS
;; ============================================================

;; macOS: Map Option → Meta, Command → Super.
;; On macOS the physical Option key is the natural Meta (M-) key.
;; Mapping Command to Super leaves it free for macOS system shortcuts
;; (Cmd-C/V/Tab etc.) while giving Emacs a Super (s-) modifier for
;; custom bindings if needed.
(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; Windows: Map the Windows key to Super.
;; By default Windows intercepts the Windows key before Emacs sees it.
;; Setting w32-pass-rwindow-to-system nil lets Emacs receive it so you
;; can bind s-<key> shortcuts.
(when IS-WIN
  (setq w32-pass-rwindow-to-system nil
        w32-rwindow-modifier 'super))

;; ============================================================
;; 9. FONTS
;; ============================================================

;; Enable Color Emoji support per OS.
;; Emacs's default font doesn't cover emoji codepoints, so it falls back to
;; whatever the font backend finds first — often a monochrome system font.
;; We explicitly prepend the OS's color emoji font so emoji render in full
;; color in org files, commit messages, and buffers containing CJK+emoji text.
(when IS-WIN
  (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))

(when IS-MAC
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend))

(when IS-LINUX
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend))
