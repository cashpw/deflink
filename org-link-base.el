;;; org-link-base.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cash Weaver
;;
;; Author: Cash Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Weaver <cashbweaver@gmail.com>
;; Created: March 13, 2022
;; Modified: March 13, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/cashweaver/org-link-base
;; Package-Requires: ((emacs "27.1"))
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

(defun org-link-base--export-markdown (path description info)
  "Format the link for exporting to markdown.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (let ((description
         (or
          description
          path)))
    (s-format
     "[${description}](${path})"
     'aget
     `(("description" . ,description)
       ("path" . ,path)))))

(defun org-link-base--export-html (path description info)
  "Format the link for exporting to html.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (let ((description
         (or
          description
          path)))
    (s-format
     "<a href=\"${path}\">${description}</a>"
     'aget
     `(("description" . ,description)
       ("path" . ,path)))))

(defun org-link-base--export-latex (path description info)
  "Format the link for exporting to latex.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (if description
      (s-format
       "\\href{${path}}{${description}}"
       'aget
       `(("description" . ,description)
         ("path" . ,path)))
    (s-format
     "\\url{${path}}"
     'aget
     `(("path" . ,path)))))

(defun org-link-base--export-ascii (path description info)
  "Format the link for exporting to ascii.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (if description
      (concat
       (format
        "[%s]"
        description)
       (and
        (not
         (plist-get info :ascii-links-to-notes))
        (format
         " (<%s>)"
         path)))
    (format
     "<%s>"
     path)))

(defun org-link-base--export-texinfo (path description info)
  "Format the link for exporting to texinfo.

- PATH: The uri/etc.
- DESCRIPTION: The reader-facing description.
- INFO: a plist containing the export parameters."
  (if description
      (s-format
       "@uref{${path}, ${description}}"
       'aget
       `(("description" . ,description)
         ("path" . ,path)))
    (s-format
     "@uref{${path}}"
     'aget
     `(("path" . ,path)))))

(defun org-link-base--build-uri (base-url path)
  "Return a uri for the provided PATH and BASE-URL."
  (url-encode-url
   (s-format
    "${base-url}/${path}"
    'aget
    `(("base-url" . ,base-url)
      ("path" . ,path)))))

(defun org-link-base-export-fn-builder (base-url)
  "Return a function to open the provided link in the format expected by `org-link-set-parameters''s `:export'.

- BASE-URL: The part of the URL not included in the path."
  (lambda (path desc backend info)
    "Export an base link.

- PATH: the name.
- DESC: the description of the link, or nil.
- BACKEND: a symbol representing the backend used for export.
- INFO: a plist containing the export parameters."
    (let ((uri
           (org-link-base--build-uri
            base-url
            path)))
      (pcase backend
        (`md
         (org-link-base--export-markdown uri desc info))
        (`html
         (org-link-base--export-html uri desc info))
        (`latex
         (org-link-base--export-latex uri desc info))
        (`ascii
         (org-link-base--export-ascii uri desc info))
        (`texinfo
         (org-link-base--export-texinfo uri desc info))
        (_ uri)))))

(defun org-link-base-open-fn-builder (base-url)
  "Return a function to open the provided link in the format expected by `org-link-set-parameters''s `:follow'.

- BASE-URL: The part of the URL not included in the path."
  (lambda (path arg)
    (let ((uri
           (org-link-base--build-uri
            base-url
            path)))
      (browse-url
       uri
       arg))))

(defun org-link-base--link-prefix (prefix)
  "Return a link PREFIX."
  (concat
   prefix
   ":"))

(defun org-link-base--get-link-path (link prefix)
  "Return the PREFIX component of the provided LINK.

(org-link-base--get-link-path \"foo:bar\" \"foo\")
=> \"bar\""
  (string-remove-prefix
   (org-link-base--link-prefix prefix)
   link))

(defun org-link-base--call-when-link-matches (fn link-type link)
  "Invokes the provided FN when LINK is of type LINK-TYPE.

Useful for implementing `org-link-make-description'."
  (lexical-let ((link link)
                (description description))
    (when (string-prefix-p
           (org-link-base--link-prefix link-type)
           link)
      (let ((path
             (s-chop-suffix
              "/"
              (org-link-base--get-link-path))))
        (funcall fn path)))))

(provide 'org-link-base)
;;; org-link-base.el ends here
