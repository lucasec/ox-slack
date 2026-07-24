;;; ox-slack.el --- Slack Exporter for org-mode -*- lexical-binding: t; -*-


;; Copyright (C) 2018 Matt Price

;; Author: Matt Price
;; Keywords: org, slack, outlines
;; Package-Version: 0.1.1
;; Package-Requires: ((emacs "24") (org "9.1.4") (ox-gfm "1.0"))
;; URL: https://github.com/titaniumbones/ox-slack

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Slack backend for the Org
;; exporter, based on the `md' and `gfm' back-ends.

;;; Code:

(require 'ox-gfm)

(org-export-define-derived-backend 'slack 'gfm
  ;; for now, I just have this commented out
  ;; might be better to create a defcustom to
  ;; decide whether to add this to the export dispatcher
  ;; :menu-entry
  ;; '(?s "Export to Slack syntax"
  ;;      ((?s "To temporary buffer"
  ;;           (lambda (a s v b) (org-slack-export-as-slack a s v)))
  ;;       (?S "To file" (lambda (a s v b) (org-slack-export-to-slack a s v)))
  ;;       (?o "To file and open"
  ;;           (lambda (a s v b)
  ;;             (if a (org-slack-export-to-slack t s v)
  ;;               (org-open-file (org-slack-export-to-slack nil s v)))))))
  :filters-alist
  '((:filter-final-output . org-slack-trim-final-output))
  :translate-alist
  '(
    (bold . org-slack-bold)
    (code . org-slack-code)
    (headline . org-slack-headline)
    (inner-template . org-slack-inner-template)
    (italic . org-slack-italic)
    (link . org-slack-link)
    (plain-text . org-slack-plain-text)
    (src-block . org-slack-src-block)
    (strike-through . org-slack-strike-through)
    (timestamp . org-slack-timestamp)))

(defun org-slack-trim-final-output (text _backend _info)
  "Trim leading and trailing blank lines from the whole export TEXT."
  (string-trim text))

;; timestamp
(defun org-slack-timestamp (timestamp _contents _info)
  "Transcode a TIMESTAMP object into Slack format.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  ;; Render as a inline code to increase visibility, but drop the <>
  ;; and [] delimiters.
  (format "`%s`"
          (replace-regexp-in-string
           "[][<>]" "" (org-timestamp-translate timestamp))))

;; headline
(defun org-slack-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (concat "     " (org-make-tag-string tag-list))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title)))
      (format "*%s*\n\n%s" title contents)
      )))

;; link
(defun org-slack-link (link contents info)
  "Transcode LINK object into Slack format.
CONTENTS is the link's description.  INFO is a plist used as a
communication channel."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (desc (org-string-nw-p contents)))
    (cond
     ;; Routable web links: Use Slack URL syntax if has a description
     ((member type '("http" "https" "ftp"))
      (let ((url (concat type ":" path)))
        (if desc (format "[%s](%s)" desc url) url)))
     ;; Email addresses: use a mailto link if has a description
     ((string= type "mailto")
      (if desc (format "[%s](mailto:%s)" desc path) path))
     ;; References to local files, other locations
     ((member type '("file" "custom-id" "id" "fuzzy"))
      ;; Resolve the link to its destination, either the path to
      ;; another local file, or a headline/location
      (let ((dest (pcase type
                    ("file" path)
                    ("fuzzy" (org-export-resolve-fuzzy-link link info))
                    (_ (org-export-resolve-id-link link info)))))
        (pcase (org-element-type dest)
          ;; Local file path: just emit the link description, or if no
          ;; description the base name of the file.  These can't
          ;; actually link anywhere useful and leaking our internal
          ;; directory structure just adds noise.
          (`plain-text
           (format "*%s*"
                   (or desc (file-name-nondirectory (directory-file-name dest)))))
          ;; Headline within the export region: Slack doesn't have
          ;; anchors, so just emit the description or headline title
          ;; in bold.  If the description doesn't match the title,
          ;; include the headline title too.
          (`headline
           (let ((title (org-export-data (org-element-property :title dest) info)))
             (if (and desc (not (string= desc title)))
                 (format "*%s* [%s]" desc title)
               (format "*%s*" (or desc title)))))
          ;; Exotic targets (tables, figures): emit the description,
          ;; or else the target's ordinal number in [brackets].
          (_
           (or (and desc (format "*%s*" desc))
               (let ((number (org-export-get-ordinal dest info)))
                 (and number
                      (format "[%s]"
                              (if (atom number) (number-to-string number)
                                (mapconcat #'number-to-string number "."))))))))))
     ;; Coderef
     ((string= type "coderef")
      (format (org-export-get-coderef-format path contents)
              (org-export-resolve-coderef path info)))
     ;; Radio target
     ((string= type "radio") contents)
     ;; Custom types with an export handler: Call the type's own
     ;; exporter, suggesting it export Markdown (which will hopefully
     ;; be compatible).
     ((org-export-custom-protocol-maybe link contents 'md))
     ;; Any other type (gnus, etc.): keep just the description in bold
     (desc (format "*%s*" desc))
     (t path))))

(defun org-slack-verbatim (_verbatim contents _info)
  "Transcode VERBATIM from Org to Slack.
  CONTENTS is the text with bold markup. INFO is a plist holding
  contextual information."
  (format "`%s`" contents))

(defun org-slack-code (code _contents info)
  "Return a CODE object from Org to SLACK.
  CONTENTS is nil.  INFO is a plist holding contextual
  information."
  (format "`%s`"
          (org-element-property :value code)))

  ;;;; Italic

(defun org-slack-italic (_italic contents _info)
  "Transcode italic from Org to SLACK.
  CONTENTS is the text with italic markup.  INFO is a plist holding
  contextual information."
  (format "_%s_" contents))

  ;;; Bold
(defun org-slack-bold (_bold contents _info)
  "Transcode bold from Org to SLACK.
  CONTENTS is the text with bold markup.  INFO is a plist holding
  contextual information."
  (format "*%s*" contents))


;;;; Strike-through
(defun org-slack-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to SLACK.
  CONTENTS is text with strike-through markup.  INFO is a plist
  holding contextual information."
  (format "~%s~" contents))


(defun org-slack-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to SLACK.
  CONTENTS holds the contents of the item.  INFO is a plist holding
  contextual information."
  (format "`%s`"
          (org-element-property :value inline-src-block)))

;;;; Src Block
(defun org-slack-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown format.
  CONTENTS is nil. INFO is a plist used as a communication
  channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```"  "\n"))
         (suffix "```"))
    (concat prefix code suffix)))

;;;; Quote Block
(defun org-slack-quote-block (_quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to SLACK.
  CONTENTS holds the contents of the block.  INFO is a plist
  holding contextual information."
  (org-slack--indent-string contents (plist-get info :slack-quote-margin)))


(defun org-slack-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
  CONTENTS is the transcoded contents string.  INFO is a plist
  holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  (concat
   ;; Table of contents.
   ;; (let ((depth (plist-get info :with-toc)))
   ;;   (when depth
   ;;     (concat (org-md--build-toc info (and (wholenump depth) depth)) "\n")))
   ;; Document contents.
   contents
   "\n"
   ;; Footnotes section.
   (org-md--footnote-section info)))

;;;; Plain text
(defun org-slack-plain-text (text info)
  "Transcode a TEXT string into Markdown format.
  TEXT is the string to transcode.  INFO is a plist holding
  contextual information."
  ;; (when (plist-get info :with-smart-quotes)
  ;;   (setq text (org-export-activate-smart-quotes text :html info)))
  ;; The below series of replacements in `text' is order sensitive.
  ;; Protect `, *, _, and \
  ;; (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-md-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; ;; Handle special strings, if required.
  ;; (when (plist-get info :with-special-strings)
  ;;   (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)

;;; End-user functions

;;;###autoload
(defun org-slack-export-as-slack
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

  If narrowing is active in the current buffer, only export its
  narrowed part.

  If a region is active, export that region.

  A non-nil optional argument ASYNC means the process should happen
  asynchronously.  The resulting buffer should be accessible
  through the `org-export-stack' interface.

  When optional argument SUBTREEP is non-nil, export the sub-tree
  at point, extracting information from the headline properties
  first.

  When optional argument VISIBLE-ONLY is non-nil, don't export
  contents of hidden elements.

  When optional argument BODY-ONLY is non-nil, strip title and
  table of contents from output.

  EXT-PLIST, when provided, is a property list with external
  parameters overriding Org default settings, but still inferior to
  file-local settings.

  Export is done in a buffer named \"*Org SLACK Export*\", which
  will be displayed when `org-export-show-temporary-export-buffer'
  is non-nil."
  (interactive)
  (org-export-to-buffer 'slack "*Org SLACK Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

;;;###autoload
(defun org-slack-export-to-slack
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text file.

  If narrowing is active in the current buffer, only export its
  narrowed part.

  If a region is active, export that region.

  A non-nil optional argument ASYNC means the process should happen
  asynchronously.  The resulting file should be accessible through
  the `org-export-stack' interface.

  When optional argument SUBTREEP is non-nil, export the sub-tree
  at point, extracting information from the headline properties
  first.

  When optional argument VISIBLE-ONLY is non-nil, don't export
  contents of hidden elements.

  When optional argument BODY-ONLY is non-nil, strip title and
  table of contents from output.

  EXT-PLIST, when provided, is a property list with external
  parameters overriding Org default settings, but still inferior to
  file-local settings.

  Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".txt" subtreep)))
    (org-export-to-file 'slack file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-slack-export-to-clipboard-as-slack ()
  "Export region to slack, and copy to the kill ring for pasting into other programs."
  (interactive)
  (let* ((org-export-with-toc nil)
         (org-export-with-smart-quotes nil))
    (kill-new (org-export-as 'slack) ))
  )

;; (org-export-register-backend 'slack)
(provide 'ox-slack)

;; Local variables:
;; coding: utf-8
;; End:

;;; ox-slack.el ends here
