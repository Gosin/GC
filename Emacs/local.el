;;; local.el --- Machine-specific configuration -*- lexical-binding: t -*-
;;
;; This file is loaded in section 2.5 of init.el, after the package system
;; is ready but before any package configuration runs.
;; Do NOT commit this file to version control — each machine has its own copy.
;;
;; ============================================================

;; ============================================================
;; MACHINE IDENTITY (optional, for conditional logic below)
;; ============================================================

;; Give this machine a name so you can write (when (eq my/machine 'work) ...)
;; blocks for finer-grained overrides anywhere in your config.
;; Suggested values: 'macbook-work  'desktop-home  'linux-server  'windows-work
(defvar my/machine 'ubuntu
  "Symbol identifying this machine. Used for conditional configuration.")

;; ============================================================
;; ORG: CORE PATHS
;; ============================================================

;; Root directory for all org files.
;; The init.el `(bound-and-true-p org-directory)' check will pick this up
;; and skip its own fallback logic.
(setq org-directory "~/Garage/gnotes/Org")

;; Default capture target — where quick notes land before refiling.
;; The init.el `(bound-and-true-p org-default-notes-file)' check will pick
;; this up and skip its own fallback logic.
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;; ============================================================
;; ORG: AGENDA
;; ============================================================

;; Explicit agenda file list for this machine.
;; Paths are computed from org-directory so they move with it.
(setq org-agenda-files
      (mapcar (lambda (f) (expand-file-name f org-directory))
              '("TAOCP.org"
                "gosin.org"
                "home.org"
                "note.org"
                "wyatterp_wolfram.org")))

;; Include the Emacs diary in the agenda (holidays, anniversaries, etc.).
(setq org-agenda-include-diary t)

;; Default agenda time span: 'day or 'week.
(setq org-agenda-span 'day)

;; Week start day: 0 = Sunday, 1 = Monday.
(setq org-agenda-start-on-weekday 1)

;; ============================================================
;; ORG: CAPTURE TEMPLATES
;; ============================================================

;; All capture file paths are computed from org-directory so they work
;; regardless of where org-directory points on this machine.
(setq org-capture-templates
      `(("b" "添加一本图书")
        ("m" "添加一部影视作品")
        ("mt" "添加一部想看的影视作品" entry
         (file ,(expand-file-name "movies.org" org-directory))
         "* TODO %?\n  %U" :prepend t)
        ("md" "添加一部看过的电影" entry
         (file ,(expand-file-name "movies.org" org-directory))
         "* DONE %?\n  %U")
        ("mi" "添加一部正在看的电视剧" entry
         (file ,(expand-file-name "movies.org" org-directory))
         "* WATCHING %?\n  %U" :prepend t :empty-lines-after 1)
        ("bd" "添加一本读过的图书" entry
         (file ,(expand-file-name "books.org" org-directory))
         "* DONE %?\n  %U")
        ("bt" "添加一本想读的图书" entry
         (file ,(expand-file-name "books.org" org-directory))
         "* TODO %?\n  %U")
        ("bi" "添加一本正在阅读的图书" entry
         (file ,(expand-file-name "books.org" org-directory))
         "* READING %?\n  %U"
         :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("t" "Add a TODO item" entry
         (file ,(expand-file-name "note.org" org-directory))
         "* TODO %?%U%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("w" "Add a Wyatt task" entry
         (file+headline ,(expand-file-name "wyatterp_wolfram.org" org-directory) "Backlogs")
         "* TODO %?" :empty-lines-after 1)))

;; ============================================================
;; ORG: REFILE
;; ============================================================

;; Expose headings up to depth 6 as refile targets.
;; Deeper than the default 3 because your projects have nested subheadings.
(setq org-refile-targets '((org-agenda-files :maxlevel . 6)))

;; Show the full file+outline path when picking a refile target.
;; 'file means the filename is shown as the first path component,
;; making it easy to distinguish same-named headings across files.
(setq org-refile-use-outline-path 'file)

;; Allow creating a new parent heading during refile (with confirmation).
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; ============================================================
;; ORG: LOGGING & DRAWERS
;; ============================================================

;; Record a timestamp when a TODO is marked DONE.
(setq org-log-done 'time)

;; Store state-change notes and clock entries in a LOGBOOK drawer
;; instead of inline under the heading, keeping headings uncluttered.
(setq org-log-into-drawer "LOGBOOK")

;; ============================================================
;; ORG: APPEARANCE
;; ============================================================

;; Hide the *bold*, /italic/, =code= markers — show only the styled text.
(setq org-hide-emphasis-markers t)

;; Render LaTeX entities (e.g. \alpha → α) and pretty-print symbols.
(setq org-pretty-entities t)

;; Require curly braces for sub/superscripts: a_{b} works, a_b does not.
;; Prevents false positives like org_mode being rendered as org<sub>mode</sub>.
(setq org-use-sub-superscripts '{})

;; Align tags to this column. Adjust to match your typical window width.
(setq org-tags-column -80)

;; Inline image display width. nil = respect #+ATTR_ORG :width.
(setq org-image-actual-width nil)

;; Column at which the habit graph starts in the agenda.
(setq org-habit-graph-column 50)

;; ============================================================
;; ORG: BABEL
;; ============================================================

;; Declare which languages to activate for org-babel on this machine.
;; Do NOT call org-babel-do-load-languages here — org is not loaded yet
;; at the point local.el runs. init.el's org :config block reads this
;; variable and calls org-babel-do-load-languages at the right time.
(setq my/org-babel-languages
      '((emacs-lisp . t)
        (shell      . t)
        ;; (python  . t)  ; uncomment once python3 confirmed available
        ))

;; Skip the "Do you want to evaluate this code block?" confirmation prompt.
;; Safe because you control which languages are enabled above.
(setq org-confirm-babel-evaluate nil)

;; Python interpreter used by babel src blocks.
(setq org-babel-python-command "python3")

;; ============================================================
;; ORG: ARCHIVE
;; ============================================================

;; Context information saved alongside each archived entry.
(setq org-archive-save-context-info '(time file category todo itags olpath))

;; ============================================================
;; ORG: MODULES
;; ============================================================

;; Load the org modules needed on this machine.
;; org-mac-link and org-notify are macOS-specific — remove on Linux/Windows.
(setq org-modules
      '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus
        org-habit org-id ol-info ol-irc ol-mhe org-protocol
        ol-rmail org-tempo ol-w3m ol-eshell ol-git-link
        org-mac-link org-notify))

;; ============================================================
;; ORG: EXPORT
;; ============================================================

;; Backends to load. Must be set before org loads (which happens in init.el's
;; use-package block after this file). Remove 'latex if no LaTeX is installed.
(setq org-export-backends '(html latex md odt))

;; LaTeX compiler. pdflatex is the default; switch to xelatex for CJK/Unicode.
(setq latex-run-command "pdflatex")

;; ============================================================
;; ORG: MOBILE (optional)
;; ============================================================

;; Uncomment and set if you use MobileOrg or a similar sync app.
;; (setq org-mobile-directory "~/Dropbox/MobileOrg")
;; (setq org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-directory))

;; ============================================================
;; FONTS & FACES
;; ============================================================

;; Primary monospaced font and size.
;; Height is in 1/10pt units: 140 = 14pt.
;; Adjust family and height to match this machine's display DPI.
(set-face-attribute 'default nil
                    :family "Berkeley Mono"
                    :height 140
                    :width 'normal)

;; Custom cursor: bright yellow, highly visible against dark themes.
(set-face-attribute 'cursor nil
                    :background "yellow1"
                    :foreground "#002b36"
                    :inverse-video t)

;; Override the ANSI blue color to a more visible sky-blue.
;; The default ANSI blue is too dark to read on dark backgrounds.
;; Deferred to after-init-hook because ansi-color faces don't exist until
;; the ansi-color package is loaded, which happens after local.el runs.
;; Calling set-face-attribute on a non-existent face causes a startup error.
(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'ansi-color-blue nil
                                :background "DeepSkyBlue1"
                                :foreground "DeepSkyBlue1")))

;; Config gptel to use llama.cpp running on Homelab
(defun my/llama-fetch-models (host protocol)
  "Fetch available models from llama-server via /v1/models."
  (let* ((url (format "%s://%s/v1/models" protocol host))
         (response (with-current-buffer (url-retrieve-synchronously url t)
                     (goto-char (point-min))
                     (re-search-forward "^$")
                     (json-parse-buffer :object-type 'plist)))
         (data (plist-get response :data)))
    (mapcar (lambda (m) (intern (plist-get m :id))) data)))

(defun my/setup-llama-backend ()
  "Set up llama-cpp backend by fetching models from the server."
  (let* ((host "192.168.50.101:11434")
         (protocol "http")
         (models (condition-case nil
                     (my/llama-fetch-models host protocol)
                   (error '(Qwen_Qwen3-14B-Q5_K_M))))) ;; fallback if server is down
    (setq gptel-backend
          (gptel-make-openai "llama-cpp"
            :stream t
            :protocol protocol
            :host host
            :models models))
    (setq gptel-model 'Qwen_Qwen3-14B-Q5_K_M)))

;; Run after init so the server has time to be reachable
(add-hook 'emacs-startup-hook #'my/setup-llama-backend)

;; ============================================================
;; EXAMPLE: CONDITIONAL BLOCKS BY MACHINE
;; ============================================================

;; Use the my/machine symbol defined at the top for anything that differs
;; substantially between machines.

;; (when (eq my/machine 'macbook-work)
;;   (setq org-agenda-files ...)
;;   (setq org-clock-idle-time 10))

;; (when (eq my/machine 'desktop-home)
;;   (setq org-clock-idle-time 20)
;;   (set-face-attribute 'default nil :height 160))  ; larger font on 4K display

;;; local.el ends here
