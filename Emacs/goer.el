;;; Goer means Gosin Ergonomics.
;;; Some personal functions to help me.

(defun create-shell (directory &optional name)
  "Create a shell buffer with given directory and name if specified."
  (if (and (stringp directory)
           (directory-name-p directory))
      (progn
        (eshell)
        (cd directory)
        (if (stringp name)
            (rename-buffer name)))))


(defun gosin-routine ()
  "Some routing operations to be done after starting Emacs."
  (progn
    (split-window-right)
    (split-window-below)
    (create-shell "/Users/gosin/Garage/s3s/" "splatoon")
    (create-shell "/Users/gosin/Garage/cppTest/" "cpp-shell")
    (create-shell "/Users/gosin/Garage/tree-sitter-wolfram/" "wolfram-shell")
    (create-shell "/Users/gosin/Garage/rust_concurrency/" "rust-shell")
    (find-file "/Users/gosin/Garage/tree-sitter-wolfram/grammar.js")
    (find-file "/Users/gosin/Documents/Org/movies.org")
    (find-file "/Users/gosin/Documents/Org/books.org")
    (find-file "/Users/gosin/Documents/Org/Emacs.org")
    (switch-to-buffer "*scratch*")))
