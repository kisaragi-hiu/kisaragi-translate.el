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

(defmacro gettext-parser--concat! (place str)
  "Concat STR to PLACE in-place."
  (macroexp-let2 macroexp-copyable-p x str
    (gv-letplace (getter setter) place
      (funcall setter `(concat ,getter ,x)))))

(defun gettext-parser--char-at (str index)
  "Return the character at INDEX in STR, as a string."
  (substring str index (min (1+ index)
                            (length str))))

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
                      (gettext-parser--char-at str (+ pos (length current-line))))))
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

;;;; PO parser

(defvar-local gettext-parser--po--validation nil)
(defvar-local gettext-parser--po--line-number nil)
(defvar-local gettext-parser--po--state 'none
  "The current parsing state.
Possible values: `none', `comments', `key', `string', `obsolete'.")
(defvar-local gettext-parser--po--node nil)
(defvar-local gettext-parser--po--escaped nil)
(defvar-local gettext-parser--po--lex nil
  "Token parsing state.")
(defconst gettext-parser--po-types '(comments key string obsolete)
  "Possible values: `comments', `key', `string', `obsolete'.")
(defconst gettext-parser--po-symbols
  `((quotes . "[\"']")
    (comments . "#")
    (whitespace . "[[:space:]]")
    (key . ,(rx (any "-" word "\\]")))
    (key-names . ,(rx bol
                      (or "msgctxt"
                          "msgid"
                          "msgid_plural"
                          (seq "msgstr"
                               (opt "[" (+ digit) "]")))
                      eol)))
  "String matches for lexer.")

(defmacro gettext-parser--po--concat-node-value! (value)
  "Concat VALUE to node.value."
  `(gettext-parser--concat!
    (map-elt gettext-parser--po--node 'value)
    ,value))
;; Parser.prototype._lexer
;; TODO: push order
(defun gettext-parser--po-lexer (chunk)
  "Token parser.
Parsed state is stored into `gettext-parser--po-lex'.
CHUNK is a string for the chunk to process."
  (let ((len (length chunk))
        (i 0)
        chr)
    ;; Use an index like this to allow backtracking
    (while (< i len)
      (setq chr (string (aref chunk i)))
      (when (equal chr "\n")
        (cl-incf gettext-parser--po--line-number))
      (pcase gettext-parser--po--state
        ((or 'none 'obsolete)
         (cond ((string-match-p
                 (map-elt gettext-parser--po-symbols 'quotes)
                 chr)
                (setq gettext-parser--po--node
                      `((type . string)
                        (value . "")
                        (quote . ,chr)))
                (push gettext-parser--po--node
                      gettext-parser--po--lex)
                (setq gettext-parser--po--state 'string))
               ((string-match-p
                 (map-elt gettext-parser--po-symbols 'comments)
                 chr)
                (setq gettext-parser--po--node
                      `((type . comments)
                        (value . "")))
                (push gettext-parser--po--node
                      gettext-parser--po--lex)
                (setq gettext-parser--po--state 'comments))
               ((string-match-p
                 (map-elt gettext-parser--po-symbols 'whitespace)
                 chr)
                (setq gettext-parser--po--node
                      `((type . key)
                        (value . ,chr)
                        ,@(when (eq gettext-parser--po--state 'obsolete)
                            '((obsolete . t)))))
                (push gettext-parser--po--node
                      gettext-parser--po--lex)
                (setq gettext-parser--po--state 'key))))
        ('comments
         (cond ((equal chr "\n")
                (setq gettext-parser--po--state 'none))
               ((and (equal chr "~")
                     (equal (map-elt gettext-parser--po--node 'value)
                            ""))
                (gettext-parser--po--concat-node-value! chr)
                (setq gettext-parser--po--state 'obsolete))
               ((not (equal chr "\r"))
                (gettext-parser--po--concat-node-value! chr))))
        ('string
         (cond
          (gettext-parser--po--escaped
           (pcase chr
             ("t" (gettext-parser--po--concat-node-value! "\t"))
             ("n" (gettext-parser--po--concat-node-value! "\n"))
             ("r" (gettext-parser--po--concat-node-value! "\r"))
             (_ (gettext-parser--po--concat-node-value! chr)))
           (setq gettext-parser--po--escaped nil))
          ((equal chr "\\")
           (setq gettext-parser--po--escaped t))
          (t
           (cond ((equal chr (map-elt gettext-parser--po--node 'quote))
                  (setq gettext-parser--po--state 'none))
                 (t
                  (gettext-parser--po--concat-node-value! chr)))
           (setq gettext-parser--po--escaped nil))))
        ('key
         (if (string-match-p
              (map-elt gettext-parser--po-symbols 'key)
              chr)
             (gettext-parser--po--concat-node-value! chr)
           (unless (string-match-p
                    (map-elt gettext-parser--po-symbols 'key-names)
                    (map-elt gettext-parser--po--node 'value))
             (error "Error parsing PO data: Invalid key name \"%s\" at line %s. This can be caused by an unescaped quote character in a msgid or msgstr value"
                    (map-elt gettext-parser--po--node 'value)
                    gettext-parser--po--line-number))
           (setq gettext-parser--po--state 'none)
           (cl-decf i))))
      (cl-incf i))))

;; Parser.prototype._joinStringValues
(defun gettext-parser--po-join-string-values (tokens)
  "Join multiline strings in TOKENS."
  (let (response last-node)
    (dolist (node tokens)
      (cond ((and last-node
                  (eq (map-elt node 'type) 'string)
                  (eq (map-elt last-node 'type) 'string))
             (gettext-parser--concat!
              (map-elt last-node 'value)
              (map-elt node 'value)))
            ((and last-node
                  (eq (map-elt node 'type) 'comments)
                  (eq (map-elt last-node 'type) 'comments))
             (gettext-parser--concat!
              (map-elt last-node 'value)
              (concat "\n" (map-elt node 'value))))
            (t
             (push node response)
             (setq last-node node))))
    (nreverse response)))

;; Parser.prototype._parseComments
;; TODO: push order
(defun gettext-parser--po-parse-comments (tokens)
  "Parse comments in TOKENS into separate comment blocks."
  (dolist (node tokens)
    (when (and node (eq (map-elt node 'type)
                        'comments))
      (let ((comment '((translator . nil)
                       (extracted . nil)
                       (reference . nil)
                       (flag . nil)
                       (previous . nil)))
            (lines (split-string
                    (or (map-elt node 'value) "")
                    "\n")))
        (dolist (line lines)
          (pcase (gettext-parser--char-at line 0)
            (":" (push (string-trim (substring line 1))
                       (map-elt comment 'reference)))
            ("." (push (replace-regexp-in-string "^[[:space:]]+" ""
                                                 (substring line 1))
                       (map-elt comment 'extracted)))
            ("," (push (replace-regexp-in-string "^[[:space:]]+" ""
                                                 (substring line 1))
                       (map-elt comment 'flag)))
            ("|" (push (replace-regexp-in-string "^[[:space:]]+" ""
                                                 (substring line 1))
                       (map-elt comment 'previous)))
            ("~" nil)
            (_ (push (replace-regexp-in-string "^[[:space:]]+" "" line)
                     (map-elt comment 'translator)))))
        (setf (map-elt node 'value) nil)
        (dolist (key (map-keys comment))
          (when (map-elt comment key)
            (setf (map-elt (map-elt node 'value) key)
                  (string-join (map-elt comment key)
                               "\n"))))))))

;; Parser.prototype._handleKeys
(defun gettext-parser--po-handle-keys (tokens)
  "Join gettext keys with values in TOKENS."
  (let ((i 0)
        (len (length tokens))
        node
        response
        last-node)
    (while (< i len)
      (setq node (elt tokens i))
      (cond ((eq (map-elt node 'type) 'key)
             (setq last-node
                   `((key . ,(map-elt node 'value))
                     ,@(when (map-elt node 'obsolete)
                         '((obsolete . t)))
                     ,@(let ((prev (elt tokens (1- i))))
                         (when (and (/= i 0)
                                    (eq (map-elt prev 'type) 'comments))
                           `((comments . ,(map-elt prev 'value)))))
                     (value . "")))
             (push last-node response))
            ((and (eq (map-elt node 'type) 'string)
                  last-node)
             (gettext-parser--concat!
              (map-elt last-node 'value)
              (map-elt node 'value))))
      (cl-incf i))
    (nreverse response)))

;; Parser.prototype._handleValues
(defun gettext-parser--po-handle-values (tokens)
  "Separate different values into individual translation objects in TOKENS."
  (let (response last-node cur-context cur-comments)
    (dolist (node tokens)
      (let ((key (downcase (map-elt node 'key))))
        (cond ((equal key "msgctxt")
               (setq cur-context (map-elt node 'value))
               (setq cur-comments (map-elt node 'comments)))
              ((equal key "msgid")
               (setq last-node
                     `((msgid . ,(map-elt node 'value))
                       ,@(when (map-elt node 'obsolete)
                           '((obsolete . t)))
                       ,@(when cur-context
                           `((msgctxt . ,cur-context)))
                       ,@(cond
                          ((and (map-elt node 'comments)
                                (not (map-elt last-node 'comments)))
                           `((comments . ,(map-elt node 'comments))))
                          (cur-comments
                           `((comments . ,cur-comments))))))
               (setq cur-context nil
                     cur-comments nil)
               (push last-node response))
              ((equal key "msgid_plural")
               (when last-node
                 (when (and gettext-parser--po--validation
                            (map-elt last-node 'msgid_plural))
                   (error
                    "Multiple msgid_plural error: entry \"%s\" in \"%s\" context has multiple msgid_plural declarations"
                    (map-elt last-node 'msgid)
                    (map-elt last-node 'msgctxt "")))
                 (setf (map-elt last-node 'msgid_plural)
                       (map-elt node 'value)))
               (when (and (map-elt node 'comments)
                          (not (map-elt last-node 'comments)))
                 (setf (map-elt last-node 'comments)
                       (map-elt node 'comments)))
               (setq cur-context nil
                     cur-comments nil))
              ((equal (substring key 0 6) "msgstr")
               (when last-node
                 (setf (map-elt last-node 'msgstr)
                       (append
                        (map-elt last-node 'msgstr)
                        (list (map-elt node 'value)))))
               (when (and (map-elt node 'comments)
                          (not (map-elt last-node 'comments)))
                 (setf (map-elt last-node 'comments)
                       (map-elt node 'comments)))
               (setq cur-context nil
                     cur-comments nil)))))
    (nreverse response)))

;; TODO Parser.prototype._validateToken
;; TODO Parser.prototype._normalize
;; TODO Parser.prototype._finalize
;; TODO Parser constructor
;; TODO Parser.prototype._parse

(defun gettext-parser-po-parse ()
  (gettext-parser--po-finalize
   (gettext-parser--po-lexer)))
(defun gettext-parser-po-compile ())
(defun gettext-parser-mo-parse ())
(defun gettext-parser-mo-compile ())

(provide 'gettext-parser)

;;; gettext-parser.el ends here
