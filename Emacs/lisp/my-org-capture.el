;;; my-org-capture.el --- Personal org-capture and org-roam templates -*- lexical-binding: t; -*-

(require 'org-capture)
(require 'org-roam)
(require 'douban-org)

;; ====================================================================
;; 1. Directory and File Variables
;; ====================================================================
(defvar my/org-dir
  "/home/gosin/Garage/gnotes/Org/"
  "Directory for my Org files.")

(defvar my/org-movies-file
  (expand-file-name "movies.org" my/org-dir)
  "Org file for movies and TV shows.")

(defvar my/org-books-file
  (expand-file-name "books.org" my/org-dir)
  "Org file for books.")

(defvar my/org-note-file
  (expand-file-name "note.org" my/org-dir)
  "General notes and TODO file.")

(defvar my/org-wyatt-file
  (expand-file-name "wyatterp_wolfram.org" my/org-dir)
  "Wyatt ERP work notes file.")

;; Organic tag completion configuration
(setq org-tag-alist nil)
(setq org-tags-column 0)

;; ====================================================================
;; 2. Org-Roam Dynamic Project Tracking Logic
;; ====================================================================
(defvar my-org-roam-project-list
  '("Soloist" 
    "HomeLab")
  "List of active projects for org-roam capture templates.")

(defvar my-current-captured-project nil
  "Internal variable to store the project name selected during the capture process.")

(defun my-org-roam-prompt-project ()
  "Prompts the user to select a project from `my-org-roam-project-list`.
Sets `my-current-captured-project` for use in subsequent template expansion steps."
  (setq my-current-captured-project
        (completing-read "Select Project: " my-org-roam-project-list nil t)))

(defun my-org-roam-project-target-path ()
  "Calculates the file path for a new project-specific note.
Prompts for the project name and returns a string in the format:
'Project/<project-name>/<timestamp>-<slug>.org'."
  (my-org-roam-prompt-project)
  (concat "Project/" my-current-captured-project "/" 
          "%<%Y%m%d%H%M%S>-${slug}.org"))

(defun my-org-roam-project-header-template ()
  "Generates the Org-mode header block for a project note.
Includes the dynamic project tag based on the user's selection."
  (concat "#+title: ${title}\n"
          (format "#+filetags: :Project:%s:\n\n" my-current-captured-project)
          "* Logged on %<%Y-%m-%d %H:%M>"))

;; ====================================================================
;; 3. Standard Org Capture Templates Configuration
;; ====================================================================
(setq org-capture-templates
      `(("b" "添加一本图书")
        ("m" "添加一部影视作品")
        ("mt" "添加一部想看的影视作品" entry (file ,my/org-movies-file) "* TODO %?\n  %U" :prepend t)
        ("md" "添加一部看过的电影" entry (file ,my/org-movies-file) "* DONE %?\n  %U")
        ("mi" "添加一部正在看的电视剧" entry (file ,my/org-movies-file) "* WATCHING %?\n  %U" :prepend t :empty-lines-after 1)
        ("bd" "添加一本读过的图书" entry (file ,my/org-books-file) "* DONE %?\n  %U")
        ("bt" "添加一本想读的图书" entry (file ,my/org-books-file) "* TODO %?\n  %U")
        ("bi" "添加一本正在阅读的图书" entry (file ,my/org-books-file) "* READING %?\n  %U" :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("t" "Add a TODO item" entry (file ,my/org-note-file) "* TODO %?%U%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("w" "Add a Wyatt task" entry (file+headline ,my/org-wyatt-file "Backlogs") "* TODO %?")))

;; ====================================================================
;; 4. Org-Roam Capture Templates Configuration
;; ====================================================================
(setq org-roam-capture-templates
      '(("d" "default" plain "\n\n%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")
         :unnarrowed t)

        ("r" "Thoughts" plain "\n\n%?"
         :if-new (file+head "thoughts/%<%Y%m%d%H%M%S>-${slug}.org" 
                            "#+title: ${title}\n#+filetags: :Thoughts_of_Xin_Guo:\n\n* Logged on %<%Y-%m-%d %H:%M>")
         :unnarrowed t)

        ("p" "Project Note" plain "\n\n%?"
         :if-new (file+head my-org-roam-project-target-path 
                            my-org-roam-project-header-template)
         :unnarrowed t)

        ("t" "Tech Notes" plain "\n\n%?"
         :if-new (file+head "tech/%<%Y%m%d%H%M%S>-${slug}.org" 
                            "#+title: ${title}\n#+filetags: %^G\n\n* Abstract / Overview\n\n* Implementation Details\n** Code Snippet\n#+begin_src \n\n#+end_src")
         :unnarrowed t)

        ("l" "LeetCode" plain 
         "\n* Problem Description\n%?\n\n* Solution Approach\n\n* Complexity Analysis\n- Time: \n- Space: \n\n* Code\n#+begin_src %^{Language|python|cpp|rust}\n\n#+end_src"
         :if-new (file+head "leetcode/%<%Y%m%d%H%M%S>-${slug}.org" 
                            "#+title: ${title}\n#+filetags: :leetcode:coding:\n\n- URL: %^{URL}\n- Solved on: %<%Y-%m-%d>")
         :unnarrowed t)

        ;; LLM Insights Template
        ("i" "LLM Insights" plain
         "\n* Prompt context\n%?\n\n* Key Takeaways\n\n* Model Response / Synthesis\n"
         :if-new (file+head "llm/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :llm:ai_insight:\n\n- Model: %^{Model|Gemini|GPT|Claude|Local-LLM}\n- Date: %<%Y-%m-%d>")
         :unnarrowed t)))

(provide 'my-org-capture)

;;; my-org-capture.el ends here
