;;; douban-org.el --- Search Douban and insert Org metadata -*- lexical-binding: t; -*-

;; Author: Xin Guo
;; Keywords: org, douban, media, books, movies
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This file provides Emacs commands for searching Douban through a local
;; backend command and inserting selected metadata into Org buffers.
;;
;; Expected backend command:
;;
;;   douban-search --type movie 霸王别姬
;;   douban-search --type book 红楼梦
;;
;; The backend should return a JSON array to stdout.
;;
;; Example JSON item:
;;
;; {
;;   "type": "movie",
;;   "id": "1291546",
;;   "title": "霸王别姬 (1993)",
;;   "url": "https://movie.douban.com/subject/1291546/",
;;   "year": "1993",
;;   "creator": null,
;;   "people": "陈凯歌 / 张国荣 / 张丰毅 / 巩俐",
;;   "rating": "9.6",
;;   "cover": "https://...",
;;   "summary": "中国大陆 / 中国香港 / 剧情 / 爱情 / ..."
;; }

;;; Code:

(require 'json)
(require 'org)
(require 'subr-x)


;;;; Customization

(defgroup my/douban nil
  "Search Douban and insert Org metadata."
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

(defcustom my/douban-insert-cover-image t
  "Whether to insert the cover image link after the Douban link."
  :type 'boolean
  :group 'my/douban)

(defcustom my/douban-insert-sections nil
  "Whether to insert standard child sections after metadata."
  :type 'boolean
  :group 'my/douban)

(defcustom my/douban-standard-sections
  '("Notes")
  "Standard child sections inserted under a media entry."
  :type '(repeat string)
  :group 'my/douban)


;;;; Backend call

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


;;;; Helpers

(defun my/douban--alist-get-string (key item)
  "Return string value for KEY in ITEM, or nil.

ITEM should be an alist parsed from the backend JSON result."
  (let ((value (alist-get key item)))
    (cond
     ((null value) nil)
     ((eq value :json-false) nil)
     ((stringp value) value)
     (t (format "%s" value)))))


(defun my/douban--nonempty-string-p (s)
  "Return non-nil if S is a non-empty string after trimming."
  (and (stringp s)
       (not (string-empty-p (string-trim s)))))


(defun my/douban--safe-property-value (s)
  "Return S normalized for use as a single-line Org property value."
  (when (my/douban--nonempty-string-p s)
    (string-trim
     (replace-regexp-in-string "[\n\r\t]+" " " s))))


(defun my/douban--format-property (key value)
  "Format Org property KEY with VALUE.

Return empty string when VALUE is nil or empty."
  (let ((safe-value (my/douban--safe-property-value value)))
    (if safe-value
        (format ":%s: %s\n" key safe-value)
      "")))


(defun my/douban--current-child-stars ()
  "Return Org stars for a child heading under the current heading.

If not currently at or inside an Org heading, default to \"**\"."
  (if (and (derived-mode-p 'org-mode)
           (save-excursion
             (ignore-errors
               (org-back-to-heading t)
               t)))
      (make-string (1+ (org-current-level)) ?*)
    "**"))


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
            (when (my/douban--nonempty-string-p year)
              (format "(%s)" year))
            (when (my/douban--nonempty-string-p rating)
              (format "★ %s" rating))
            people
            summary))
     "  ")))


(defun my/douban--select-item (type query)
  "Search Douban by TYPE and QUERY, then return the selected item."
  (let ((items (my/douban--search type query)))
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
      item)))


;;;; Org formatting

(defun my/douban--org-properties (item)
  "Format Douban ITEM as an Org property drawer."
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
     (my/douban--format-property "TYPE" type)
     (my/douban--format-property "DOUBAN_ID" id)
     (my/douban--format-property "DOUBAN_URL" url)
     (my/douban--format-property "YEAR" year)
     (cond
      ((and creator (string= type "book"))
       (my/douban--format-property "AUTHOR" creator))
      (creator
       (my/douban--format-property "DIRECTOR" creator))
      (t ""))
     (cond
      ((and people (string= type "book"))
       (my/douban--format-property "BOOK_INFO" people))
      (people
       (my/douban--format-property "PEOPLE" people))
      (t ""))
     (my/douban--format-property "RATING" rating)
     (my/douban--format-property "COVER" cover)
     (my/douban--format-property "SUMMARY" summary)
     ":END:\n")))


(defun my/douban--org-link (item)
  "Format Douban ITEM as an Org link."
  (let ((title (my/douban--alist-get-string 'title item))
        (url (my/douban--alist-get-string 'url item)))
    (if (and (my/douban--nonempty-string-p title)
             (my/douban--nonempty-string-p url))
        (format "[[%s][%s - 豆瓣]]\n" url title)
      "")))


(defun my/douban--org-cover (item)
  "Format Douban ITEM cover as an Org image link."
  (let ((cover (my/douban--alist-get-string 'cover item)))
    (if (and my/douban-insert-cover-image
             (my/douban--nonempty-string-p cover))
        (format "\n#+attr_org: :width 200\n[[%s]]\n" cover)
      "")))


(defun my/douban--org-standard-sections ()
  "Return standard child sections for a Douban Org entry."
  (if my/douban-insert-sections
      (let ((stars (my/douban--current-child-stars)))
        (concat
         "\n"
         (mapconcat (lambda (section)
                      (format "%s %s\n" stars section))
                    my/douban-standard-sections
                    "\n")))
    ""))


(defun my/douban--org-entry-body (item)
  "Format Douban ITEM as an Org entry body without the heading."
  (concat
   (my/douban--org-properties item)
   "\n"
   (my/douban--org-link item)
   (my/douban--org-cover item)
   (my/douban--org-standard-sections)))


(defun my/douban--org-block (item)
  "Format Douban ITEM as a standalone Org block.

This is useful when inserting metadata at point without modifying the current
Org heading."
  (concat
   (my/douban--org-link item)
   "\n"
   (my/douban--org-properties item)
   (my/douban--org-cover item)))


;;;; Interactive commands

(defun my/douban-search-insert (type query)
  "Search Douban by TYPE and QUERY, then insert selected Org block at point.

This command does not modify the current heading."
  (interactive
   (list
    (completing-read "Type: " '("movie" "book" "all") nil t nil nil my/douban-default-type)
    (read-string "Search Douban: ")))
  (let ((item (my/douban--select-item type query)))
    (insert (my/douban--org-block item))))


(defun my/douban-search-insert-movie (query)
  "Search Douban movie by QUERY and insert selected Org block at point."
  (interactive "sSearch Douban movie: ")
  (my/douban-search-insert "movie" query))


(defun my/douban-search-insert-book (query)
  "Search Douban book by QUERY and insert selected Org block at point."
  (interactive "sSearch Douban book: ")
  (my/douban-search-insert "book" query))


(defun my/douban-insert-entry-at-heading (type query)
  "Search Douban and insert metadata/body under the current Org heading.

This command renames the current heading to the selected Douban title and then
inserts a property drawer, Douban link, optional cover, and optional child
sections below it."
  (interactive
   (list
    (completing-read "Type: " '("movie" "book" "all") nil t nil nil my/douban-default-type)
    (read-string "Search Douban: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in org-mode"))

  (let* ((item (my/douban--select-item type query))
         (title (my/douban--alist-get-string 'title item)))
    (unless (my/douban--nonempty-string-p title)
      (user-error "Selected Douban item has no title"))

    (unless (org-at-heading-p)
      (org-back-to-heading t))

    (org-edit-headline title)
    (end-of-line)
    (insert "\n" (my/douban--org-entry-body item))))


(defun my/douban-insert-movie-at-heading (query)
  "Search Douban movie by QUERY and insert metadata under current heading."
  (interactive "sSearch Douban movie: ")
  (my/douban-insert-entry-at-heading "movie" query))


(defun my/douban-insert-book-at-heading (query)
  "Search Douban book by QUERY and insert metadata under current heading."
  (interactive "sSearch Douban book: ")
  (my/douban-insert-entry-at-heading "book" query))


(provide 'douban-org)

;;; douban-org.el ends here
