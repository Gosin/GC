;;; Goer means Gosin Ergonomics.
;;; Some personal functions to help me.

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun create-shell (directory &optional name)
  "Create a shell buffer with given directory and name if specified."
  (if (and (stringp directory)
           (directory-name-p directory))
      (progn
        (eshell)
        (cd directory)
        (if (stringp name)
            (rename-buffer name)))))

(defun my/markdown-to-org-region (start end)
  "Convert the selected Markdown region to Org mode syntax using pandoc"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))
