;;; gettext-parser.el --- Parser for PO and MO files -*- lexical-binding: t -*-

;;; Commentary:

;; Parse gettext PO files in pure Elisp.
;;
;; Port of https://github.com/smhg/gettext-parser to Emacs Lisp.
;;
;; Usage:
;; - (gettext-parser-po-parse input &key validation)
;;   Parse INPUT into a translation object.
;;   INPUT: a PO file as a buffer or a string
;;   VALIDATION: a flag to turn on PO file validation.
;;     - If there is more than one msgid_plural definition per translation
;;       entry, an error is thrown for "multiple msgid_plural".
;;     - If there are duplicate entries (same msgid value), an error is thrown
;;       for "duplicate msgid".
;;     - For entries with plural forms, if the number of plural forms does not
;;       match nplurals as defined in the Plural-Forms header, an error is
;;       thrown for "plural forms range error".
;;     - If there isn't exactly one msgstr when msgid_plural is not defined, or
;;       when the number of msgstr does not match nplurals, an error is thrown
;;       for "translation string range error".
;;   Porting note: DEFAULT-CHARSET does not apply as Emacs handles all encoding
;;   for us.
;; - (gettext-parser-po-compile data &key fold-length sort escape-characters)
;;   Turn DATA, a translation object, into valid PO format.
;;   This does NOT compile a PO file into MO!
;;   DATA: a translation object, either from parsing a PO/MO file or constructed
;;     in some other way
;;   FOLD-LENGTH: wrap lines to be under this length. 0 also disables folding,
;;     in addition to nil.
;;   SORT: whether to sort entries. Entries are sorted by "msgid". This can also
;;     be a function to compare entries.
;;   ESCAPE-CHARACTERS: set to nil to disable newline and quote escaping.
;; - (gettext-parser-mo-parse input)
;;   Parse INPUT into a translation object.
;;   INPUT: a MO file as a buffer or string
;; - (gettext-parser-mo-compile input)
;;   Turn DATA, a translation object, into valid MO.
;;
;; Data types:
;;
;; - Header: hash table representing the header
;; - Translations: hash table containing contexts.
;; - Contexts: hash table mapping entry msgids to entry values.
;;
;; The header also shows up in translations, but is just for reference.

;;; Code:

;;;; Shared
(defconst gettext-parser--headers
  `((project-id-version . "Project-Id-Version")
    (report-msgid-bugs-to . "Report-Msgid-Bugs-To")
    (pot-creation-date . "POT-Creation-Date")
    (po-revision-date . "PO-Revision-Date")
    (last-translator . "Last-Translator")
    (language-team . "Language-Team")
    (language . "Language")
    (content-type . "Content-Type")
    (content-transfer-encoding . "Content-Transfer-Encoding")
    (plural-forms . "Plural-Forms")))
(defconst gettext-parser--plural-from-header-nplurals-regexp
  (rx "nplurals" (* space) "=" (* space)
      (group (+ digit)))
  "Regexp for matching nplurals.
The first matching group is the digits for the nplurals value.")

(defun gettext-parser--parse-header (str)
  "Parse a header string STR into a header hash table."
  (let ((headers (make-hash-table :test #'equal)))
    (dolist (line (split-string str "\n"))
      (let* ((parts (split-string ":" line))
             (key (string-trim
                   (or (car parts)
                       "")))
             value)
        (unless (string-empty-p key)
          (setq value (string-trim
                       (string-join (cdr parts) ":")))
          ;; Normalize key casing
          (setq key
                (or (alist-get
                     (intern (downcase key))
                     gettext-parser--headers)
                    key))
          (puthash key value headers))))
    headers))
(cl-defun gettext-parser--parse-nplural-from-header-safely
    (&optional (header (make-hash-table :test #'equal))
               (fallback 1))
  "Attempt to safely parse nplurals value from the Plural-Forms header.
HEADER: the header hash table.
Return FALLBACK or the parsed result."
  (if-let ((plural-forms (gethash "Plural-Forms" header))
           ((string-match gettext-parser--plural-from-header-nplurals-regexp
                         plural-forms))
           (match (match-string 1)))
      (string-to-number match)
    fallback))
(cl-defun gettext-parser--generate-header
    (&optional (header (make-hash-table :test #'equal)))
  "Join the HEADER table into a header string."
  (if-let (keys (cl-remove-if #'string-empty-p (hash-table-keys header)))
      (cl-loop
       for key in keys
       concat (format "%s: %s\n"
                      key
                      (string-trim (gethash key header ""))))
    ""))
(cl-defun gettext-parser--fold-line (str &optional (max-length 76))
  "Fold long lines in STR to be at most MAX-LENGTH characters long.
Return a list of folded lines."
  (let ((lines nil)
        (len (length str))
        (current-line "")
        (pos 0))
    (while (< pos len)
      ;; porting note: JS str.substr(start, end) is inclusive whereas
      ;; `substring''s end is exclusive
      (setq current-line (substring str pos (1+ max-length)))
      ;; ensure the line never ends with partial escaping
      (while (and (equal "\\" (substring current-line -1))
                  (< (+ pos (length current-line))
                     len))
        (setq current-line
              (concat current-line
                      (string (aref str (+ pos (length current-line)))))))
      ;; A literal slash and n
      ;; Use `rx' because both string parsing and regexp parsing will handle
      ;; escaping, which means we'd have to quadruple it.
      (cond ((string-match (rx (*? any) "\\n") current-line)
             ;; Use everything before and including the first line break
             (setq current-line (match-string 0)))
            ((< (+ pos (length current-line))
                len)
             (cond
              ;; if we're not at the end
              ((and (string-match
                     (rx (* any)
                         (+ space))
                     current-line)
                    (string-match-p
                     (rx (not space))
                     (match-string 0)))
               ;; use everything before and including the last whitespace
               (setq current-line (match-string 0)))
              ((and (string-match
                     (rx (* any)
                         (+ (any "0-9\x21-\x2f\x5b-\x60\x7b-\x7e")))
                     current-line)
                    (string-match-p
                     (rx (not (any "0-9\x21-\x2f\x5b-\x60\x7b-\x7e")))
                     (match-string 0)))
               ;; use everything before and including the last special character
               (setq current-line (match-string 0))))))
      (push current-line lines)
      (cl-incf pos (length current-line)))
    (nreverse lines)))
(defun gettext-parser--compare-msgid (a b)
  "Compare entries A and B by their msgid."
  ;; TODO: how are entries represented?
  (string< (gethash a :msgid)
           (gethash b :msgid)))


(defun gettext-parser-po-parse ())
(defun gettext-parser-po-compile ())
(defun gettext-parser-mo-parse ())
(defun gettext-parser-mo-compile ())

(provide 'gettext-parser)

;;; gettext-parser.el ends here
