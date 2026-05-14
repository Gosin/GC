;;; douban-org.el --- Insert Douban metadata into Org -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)
(require 'org)

(defgroup my/douban nil
  "Search Douban and insert Org links."
  :group 'org)

(defcustom my/douban-search-command "douban-search"
  "Command used to search Douban.

The command should accept arguments like:

  douban-search --type movie 霸王别姬

and return a JSON array to stdout."
  :type 'string
  :group 'my/douban)

(defcustom my/douban-default-type "movie"
  "Default Douban search type."
  :type '(choice (const "movie")
                 (const "book")
                 (const "all"))
  :group 'my/douban)

(defun my/douban--call-process (type query)
  "Call Douban backend with TYPE and QUERY, returning raw JSON string.

Stdout and stderr are captured separately so shell warnings do not corrupt JSON."
  (unless (executable-find my/douban-search-command)
    (user-error "Cannot find command: %s" my/douban-search-command))

  (let ((stderr-file (make-temp-file "douban-search-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((exit-code
                 (call-process my/douban-search-command
                               nil
                               (list t stderr-file)
                               nil
                               "--type" type
                               query)))
            (unless (zerop exit-code)
              (let ((stderr
                     (if (file-exists-p stderr-file)
                         (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (string-trim (buffer-string)))
                       "")))
                (user-error "Douban search failed: %s" stderr)))
            (string-trim (buffer-string))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun my/douban--search (type query)
  "Search Douban for QUERY of TYPE and return a list of alists."
  (let* ((raw (my/douban--call-process type query))
         (json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'symbol))
    (json-read-from-string raw)))

(defun my/douban--alist-get-string (key item)
  "Return string value for KEY in ITEM, or nil."
  (let ((value (alist-get key item)))
    (cond
     ((null value) nil)
     ((eq value :json-false) nil)
     ((stringp value) value)
     (t (format "%s" value)))))

(defun my/douban--candidate-display (item)
  "Return completion display string for Douban ITEM."
  (let* ((type (my/douban--alist-get-string 'type item))
         (title (my/douban--alist-get-string 'title item))
         (year (my/douban--alist-get-string 'year item))
         (rating (my/douban--alist-get-string 'rating item))
         (people (my/douban--alist-get-string 'people item))
         (summary (my/douban--alist-get-string 'summary item)))
    (string-join
     (delq nil
           (list
            type
            title
            (when year (format "(%s)" year))
            (when rating (format "★ %s" rating))
            people
            summary))
     "  ")))

(defun my/douban--org-properties (item)
  "Format ITEM as an Org property drawer."
  (let* ((type (my/douban--alist-get-string 'type item))
         (id (my/douban--alist-get-string 'id item))
         (url (my/douban--alist-get-string 'url item))
         (year (my/douban--alist-get-string 'year item))
         (creator (my/douban--alist-get-string 'creator item))
         (people (my/douban--alist-get-string 'people item))
         (rating (my/douban--alist-get-string 'rating item))
         (cover (my/douban--alist-get-string 'cover item))
         (summary (my/douban--alist-get-string 'summary item)))
    (concat
     ":PROPERTIES:\n"
     (when type (format ":TYPE: %s\n" type))
     (when id (format ":DOUBAN_ID: %s\n" id))
     (when url (format ":DOUBAN_URL: %s\n" url))
     (when year (format ":YEAR: %s\n" year))
     (when creator
       (if (string= type "book")
           (format ":AUTHOR: %s\n" creator)
         (format ":DIRECTOR: %s\n" creator)))
     (when people
       (if (string= type "book")
           (format ":BOOK_INFO: %s\n" people)
         (format ":PEOPLE: %s\n" people)))
     (when rating (format ":RATING: %s\n" rating))
     (when cover (format ":COVER: %s\n" cover))
     (when summary (format ":SUMMARY: %s\n" summary))
     ":END:\n")))

(defun my/douban--org-block (item)
  "Format Douban ITEM as Org text."
  (let* ((title (my/douban--alist-get-string 'title item))
         (url (my/douban--alist-get-string 'url item))
         (cover (my/douban--alist-get-string 'cover item)))
    (concat
     (format "[[%s][%s - 豆瓣]]\n\n" url title)
     (my/douban--org-properties item)
     (when cover
       (format "\n#+attr_org: :width 200\n[[%s]]\n" cover)))))

(defun my/douban-search-insert (type query)
  "Search Douban by TYPE and QUERY, then insert selected Org metadata."
  (interactive
   (list
    (completing-read "Type: " '("movie" "book" "all") nil t nil nil my/douban-default-type)
    (read-string "Search Douban: ")))
  (let* ((items (my/douban--search type query)))
    (unless items
      (user-error "No Douban results found for: %s" query))
    (let* ((candidates
            (mapcar (lambda (item)
                      (cons (my/douban--candidate-display item) item))
                    items))
           (choice (completing-read "Douban: " candidates nil t))
           (item (cdr (assoc choice candidates))))
      (unless item
        (user-error "No Douban item selected"))
      (insert (my/douban--org-block item)))))

(defun my/douban-search-insert-movie (query)
  "Search Douban movie by QUERY and insert selected Org metadata."
  (interactive "sSearch Douban movie: ")
  (my/douban-search-insert "movie" query))

(defun my/douban-search-insert-book (query)
  "Search Douban book by QUERY and insert selected Org metadata."
  (interactive "sSearch Douban book: ")
  (my/douban-search-insert "book" query))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c d") #'my/douban-search-insert)
  (define-key org-mode-map (kbd "C-c D m") #'my/douban-search-insert-movie)
  (define-key org-mode-map (kbd "C-c D b") #'my/douban-search-insert-book))

(provide 'douban-org)

;;; douban-org.el ends here
