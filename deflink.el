;;; deflink.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cash Weaver
;;
;; Author: Cash Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Weaver <cashbweaver@gmail.com>
;; Created: March 13, 2022
;; Modified: March 17, 2024
;; Version: 0.0.2
;; Homepage: https://github.com/cashweaver/deflink
;; Package-Requires: ((emacs "27.1") (s "1.13.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This library provides org link utility functions.
;;
;;; Code:

(require 'ol)
(require 's)

(defun deflink--export-markdown (path description info)
  "Format the link for exporting to markdown.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (let ((description (or description path)))
    (s-lex-format "[${description}](${path})")))

(defun deflink--export-html (path description info)
  "Format the link for exporting to html.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (let ((description (or description path)))
    (s-lex-format "<a href=\"${path}\">${description}</a>")))

(defun deflink--export-latex (path description info)
  "Format the link for exporting to latex.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (if description
      (s-lex-format "\\href{${path}}{${description}}")
    (s-lex-format "\\url{${path}}")))

(defun deflink--export-ascii (path description info)
  "Format the link for exporting to ascii.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (if description
      (concat
       (format "[%s]" description)
       (and (not (plist-get info :ascii-links-to-notes))
            (format " (<%s>)" path)))
    (format "<%s>" path)))

(defun deflink--export-texinfo (path description info)
  "Format the link for exporting to texinfo.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (if description
      (s-lex-format "@uref{${path}, ${description}}")
    (s-lex-format "@uref{${path}}")))

(defun deflink--build-uri (uri target)
  "Return URI formatted with TARGET.

URI should be a string with a single '%s'."
  (url-encode-url (format uri target)))

(defun deflink--export-fn-builder (base-url)
  "Return a function to open the provided link in the format expected by `org-link-set-parameters''s `:export'.

- BASE-URL: The part of the URL not included in the path."
  (lambda (path desc backend info)
    "Export an base link.

- PATH: the name.
- DESC: the description of the link, or nil.
- BACKEND: a symbol representing the backend used for export.
- INFO: a plist containing the export parameters."
    (let ((uri (deflink--build-uri base-url path)))
      (pcase backend
        (`md (deflink--export-markdown uri desc info))
        (`html (deflink--export-html uri desc info))
        (`latex (deflink--export-latex uri desc info))
        (`ascii (deflink--export-ascii uri desc info))
        (`texinfo (deflink--export-texinfo uri desc info))
        (_ uri)))))

(defun deflink--open-fn-builder (base-url)
  "Return a function to open the provided link in the format expected by `org-link-set-parameters''s `:follow'.

- BASE-URL: The part of the URL not included in the path."
  (lambda (path arg)
    (let ((uri (deflink--build-uri base-url path)))
      (browse-url uri arg))))

(defun deflink--link-prefix (prefix)
  "Return a link PREFIX."
  (concat prefix ":"))

(defun deflink-link-is-type-p (link link-type)
  "Return t if LINK is prefixed with LINK-TYPE, else nil."
  (string-prefix-p (deflink--link-prefix link-type) link))

(defun deflink-call-with-path (fn link)
  "Call FN with the path component of LINK."
  (let ((link (s-chop-suffix "/" link))
        (path (deflink--get-link-path link)))
    (funcall fn path)))

(defun deflink--get-link-path (link)
  "Return path component of LINK."
  (replace-regexp-in-string "^.*?:" "" link))

(defun deflink-call-when-link-matches (fn link-type link)
  "Return the return value of FN when LINK is of type LINK-TYPE, else nil.

Useful for implementing `org-link-make-description'."
  (when (deflink-link-is-type-p link link-type)
    (deflink-call-with-path fn link))
  nil)

(defun deflink--insert-description-fn-builder (insert-description-fn)
  "Return an insert-description-type function.

See `org-link-parameters'."
  (lambda (location description)
    (let ((link (cl-second (split-string location ":"))))
      (funcall insert-description-fn link description))))

(defmacro deflink (type url &optional insert-description)
  "Define a named http TYPE link which expands to URL.

INSERT-DESCRIPTION is a function which returns a description for this link and
accepts three arguments: (1) the link (for example: \"bar\" in the link
\"foo:bar\", and (2) the description (see `org-link-parameters')."
  `(org-link-set-parameters
    ,type
    :follow (deflink--open-fn-builder ,url)
    :insert-description
    (when ,insert-description
      (deflink--insert-description-fn-builder ,insert-description))
    :export (deflink--export-fn-builder ,url)))

(provide 'deflink)
;;; deflink.el ends here
