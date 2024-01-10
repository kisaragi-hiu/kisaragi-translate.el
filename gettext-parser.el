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

;;; Code:

(require 'dash)

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
  (if-let ((plural-forms (map-elt header "Plural-Forms"))
           ((string-match gettext-parser--plural-from-header-nplurals-regexp
                          plural-forms))
           (match (match-string 1)))
      (string-to-number match)
    fallback))
(cl-defun gettext-parser--generate-header
    (&optional (header (make-hash-table :test #'equal)))
  "Join the HEADER table into a header string."
  (if-let (keys (->> (hash-table-keys header)
                     (--map (format "%s" it))
                     (-remove #'string-empty-p)))
      (cl-loop
       for key in keys
       concat (format "%s: %s\n"
                      key
                      (string-trim (map-elt header key ""))))
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
  (string< (map-elt a 'msgid)
           (map-elt b 'msgid)))

;;;; PO parser

(defun gettext-parser--translation-table ()
  "Create an empty translation table."
  (make-hash-table))
(defun gettext-parser--translation-table--elt (ttable msgctxt &optional msgid)
  "Return entries for MSGCTXT in TTABLE.
If MSGID is non-nil, lookup MSGID from the entries instead."
  (let ((entries (gethash msgctxt ttable)))
    (if (and entries msgid)
        (gethash msgid entries)
      entries)))
(defun gettext-parser--translation-table--put (ttable msgctxt entries)
  "Set MSGCTXT to ENTRIES in TTABLE."
  (cl-assert (hash-table-p entries))
  (puthash msgctxt entries ttable))

(cl-defstruct (gettext-parser--comment
               (:copier nil)
               (:constructor gettext-parser--comment))
  translator extracted reference flag previous)
(cl-defstruct (gettext-parser--node
               (:copier nil)
               (:constructor gettext-parser--node))
  comments key
  msgctxt msgid msgid_plural msgstr
  obsolete value
  ;; This is `quote', but if we name it `quote' it will error out.
  ;; This feels like a bug. Reported.
  ;; Apparently already fixed for Emacs 30.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=68345
  quot
  (type nil :documentation "The value type.
Possible values: `comments', `key', `string', `obsolete'."))

(cl-defstruct (gettext-parser--po-parser
               (:copier nil)
               (:constructor gettext-parser--po-parser))
  escaped lex line-number
  node
  validation
  (state
   nil
   :documentation "The current parsing state.
Possible values: `none', `comments', `key', `string', `obsolete'."))

(defun gettext-parser--chr-quote? (chr)
  "Whether CHR is a quotation symbol."
  (string-match-p "[\"']" chr))
(defun gettext-parser--chr-comment? (chr)
  "Whether CHR is a comment starter symbol."
  (equal "#" chr))
(defun gettext-parser--chr-whitespace? (chr)
  "Whether CHR is a whitespace symbol."
  (string-match-p "[[:space:]]" chr))
(defun gettext-parser--chr-key? (chr)
  "Whether CHR is a symbol for a key."
  (string-match-p (rx (any "-" word "\\]")) chr))
(defun gettext-parser--chr-key-name? (chr)
  "Whether CHR is a valid symbol for a key name."
  (string-match-p
   (rx bol
       (or "msgctxt"
           "msgid"
           "msgid_plural"
           (seq "msgstr"
                (opt "[" (+ digit) "]")))
       eol)
   chr))

(cl-defun gettext-parser-po-parse (input &key (validation nil))
  "Parse PO INPUT.
INPUT is either a string or a buffer.
If VALIDATION is non-nil, throw errors when there are issues."
  (let ((parser (gettext-parser--po-parser
                 :validation validation
                 :node (gettext-parser--node)
                 :state 'none
                 :line-number 1)))
    (gettext-parser--po-lexer
     parser
     (if (stringp input)
         input
       (with-current-buffer input
         (buffer-string))))
    (gettext-parser--po-finalize
     parser
     (oref parser lex))))

(defun gettext-parser--po-lexer (parser chunk)
  "Token parser.
PARSER is the parser object for storing and passing around state.
CHUNK is a string for the chunk to process."
  (let ((len (length chunk))
        (i 0)
        chr)
    ;; Use an index like this to allow backtracking
    (while (< i len)
      (setq chr (string (aref chunk i)))
      (when (equal chr "\n")
        (cl-incf (oref parser line-number)))
      (pcase (oref parser state)
        ((or 'none 'obsolete)
         (cond ((gettext-parser--chr-quote? chr)
                (setf (oref parser node)
                      (gettext-parser--node
                       :type 'string
                       :value ""
                       :quot chr))
                (push (oref parser node)
                      (oref parser lex))
                (oset parser state 'string))
               ((gettext-parser--chr-comment? chr)
                (setf (oref parser node)
                      (gettext-parser--node
                       :type 'comments
                       :value ""))
                (push (oref parser node)
                      (oref parser lex))
                (oset parser state 'comments))
               ((not (gettext-parser--chr-whitespace? chr))
                (setf (oref parser node)
                      (gettext-parser--node
                       :type 'key
                       :value chr
                       :obsolete (eq 'obsolete (oref parser state))))
                (push (oref parser node)
                      (oref parser lex))
                (oset parser state 'key))))
        ('comments
         (cond ((equal chr "\n")
                (oset parser state 'none))
               ((and (equal chr "~")
                     (equal (-> (oref parser node)
                                (oref value))
                            ""))
                (gettext-parser--concat!
                 (-> (oref parser node)
                     (oref value))
                 chr)
                (oset parser state 'obsolete))
               ((not (equal chr "\r"))
                (gettext-parser--concat!
                 (-> (oref parser node)
                     (oref value))
                 chr))))
        ('string
         (cond
          ((oref parser escaped)
           (pcase chr
             ("t" (gettext-parser--concat!
                   (-> (oref parser node)
                       (oref value))
                   "\t"))
             ("n" (gettext-parser--concat!
                   (-> (oref parser node)
                       (oref value))
                   "\n"))
             ("r" (gettext-parser--concat!
                   (-> (oref parser node)
                       (oref value))
                   "\r"))
             (_ (gettext-parser--concat!
                 (-> (oref parser node)
                     (oref value))
                 chr)))
           (oset parser escaped nil))
          ((equal chr "\\")
           (oset parser escaped t))
          (t
           (cond ((equal chr (-> (oref parser node)
                                 (oref quot)))
                  (oset parser state 'none))
                 (t
                  (gettext-parser--concat!
                   (-> (oref parser node)
                       (oref value))
                   chr)))
           (oset parser escaped nil))))
        ('key
         (if (gettext-parser--chr-key? chr)
             (gettext-parser--concat!
              (-> (oref parser node)
                  (oref value))
              chr)
           (unless (gettext-parser--chr-key-name? (-> (oref parser node)
                                                      (oref value)))
             (error "Error parsing PO data: Invalid key name \"%s\" at line %s. This can be caused by an unescaped quote character in a msgid or msgstr value"
                    (-> (oref parser node)
                        (oref value))
                    (oref parser line-number)))
           (oset parser state 'none)
           (cl-decf i))))
      (cl-incf i))
    (setf (oref parser lex)
          (nreverse (oref parser lex)))))

(defun gettext-parser--po-join-string-values (tokens)
  "Join multiline strings in TOKENS."
  (let (response last-node)
    (dolist (node tokens)
      (cond ((and last-node
                  (eq 'string (oref node type))
                  (eq 'string (oref last-node type)))
             (gettext-parser--concat!
              (oref last-node value)
              (oref node value)))
            ((and last-node
                  (eq 'comments (oref node type))
                  (eq 'comments (oref last-node type)))
             (gettext-parser--concat!
              (oref last-node value)
              (concat "\n" (oref node value))))
            (t
             (push node response)
             (setq last-node node))))
    (nreverse response)))

(defun gettext-parser--po-parse-comments (tokens)
  "Parse comments in TOKENS into separate comment blocks.
Mutates TOKENS but also returns it."
  (dolist (node tokens)
    (when (and node (eq 'comments (oref node type)))
      (let ((comment (gettext-parser--comment))
            (lines (split-string
                    (or (oref node value) "")
                    "\n")))
        (dolist (line lines)
          (pcase (gettext-parser--char-at line 0)
            (":" (push (string-trim (substring line 1))
                       (oref comment reference)))
            ("." (push (replace-regexp-in-string "^[[:space:]]+" ""
                                                 (substring line 1))
                       (oref comment extracted)))
            ("," (push (replace-regexp-in-string "^[[:space:]]+" ""
                                                 (substring line 1))
                       (oref comment flag)))
            ("|" (push (replace-regexp-in-string "^[[:space:]]+" ""
                                                 (substring line 1))
                       (oref comment previous)))
            ("~" nil)
            (_ (push (replace-regexp-in-string "^[[:space:]]+" "" line)
                     (oref comment translator)))))
        (oset node value nil)
        (dolist (key '(translator extracted reference flag previous))
          (when (eieio-oref comment key)
            (setf (eieio-oref comment key)
                  (nreverse (eieio-oref comment key)))
            (setf (eieio-oref (oref node value) key)
                  (string-join (eieio-oref comment key) "\n")))))))
  tokens)

(defun gettext-parser--po-handle-keys (tokens)
  "Join gettext keys with values in TOKENS."
  (let ((i 0)
        (len (length tokens))
        node
        response
        last-node)
    (while (< i len)
      (setq node (elt tokens i))
      (cond ((eq 'key (oref node type))
             (setq last-node
                   (gettext-parser--node
                    :key (oref node value)
                    :obsolete (oref node obsolete)
                    :comments (let ((prev (elt tokens (1- i))))
                                (and (/= i 0)
                                     (eq (oref prev type) 'comments)
                                     (oref prev value)))
                    :value ""))
             (push last-node response))
            ((and (eq 'string (oref node type))
                  last-node)
             (gettext-parser--concat!
              (oref last-node value)
              (oref node value))))
      (cl-incf i))
    (nreverse response)))

(defun gettext-parser--po-handle-values (parser tokens)
  "Separate different values into individual translation objects in TOKENS.
PARSER is the current parser object."
  (let (response last-node cur-context cur-comments)
    (dolist (node tokens)
      (let ((key (downcase (oref node key))))
        (cond ((equal key 'msgctxt)
               (setq cur-context (oref node value))
               (setq cur-comments (oref node comments)))
              ((equal key 'msgid)
               (setq last-node
                     (gettext-parser--node
                      :msgid (oref node value)
                      :obsolete (oref node obsolete)
                      :msgctxt cur-context
                      :comments (if (and (oref node comments)
                                         (not (oref last-node comments)))
                                    (oref node comments)
                                  cur-comments)))
               (setq cur-context nil
                     cur-comments nil)
               (push last-node response))
              ((equal key 'msgid_plural)
               (when last-node
                 (when (and (oref parser validation)
                            (oref last-node msgid_plural))
                   (error
                    "Multiple msgid_plural error: entry \"%s\" in \"%s\" context has multiple msgid_plural declarations"
                    (oref last-node msgid)
                    (or (oref last-node msgctxt) "")))
                 (setf (oref last-node msgid_plural)
                       (oref node value)))
               (when (and (oref node comments)
                          (not (oref last-node comments)))
                 (setf (oref last-node comments)
                       (oref node comments)))
               (setq cur-context nil
                     cur-comments nil))
              ((equal "msgstr" (substring (symbol-name key) 0 6))
               (when last-node
                 (setf (oref last-node msgstr)
                       (append
                        (oref last-node msgstr)
                        (list (oref node value)))))
               (when (and (oref node comments)
                          (not (oref last-node comments)))
                 (setf (oref last-node comments)
                       (oref node comments)))
               (setq cur-context nil
                     cur-comments nil)))))
    (nreverse response)))

(defun gettext-parser--po-validate-token (parser node translations msgctxt nplurals)
  "Validate a token, NODE.
PARSER is the parser object.

TRANSLATIONS is the translation table.
MSGCTXT is the message entry context.
NPLURALS is the number of expected plural forms.
Will throw an error if token validation fails."
  (let ((msgid (or (oref node msgid) ""))
        (msgid_plural (or (oref node msgid_plural) ""))
        (msgstr (oref node msgstr)))
    (when (oref parser validation)
      (cond
       ((gettext-parser--translation-table--elt translations msgctxt msgid)
        (error "Duplicate msgid error: entry \"%s\" in \"%s\" context has already been declared"
               msgid msgctxt))
       ((and msgid_plural
             (/= nplurals (length msgstr)))
        (error "Plural forms range error: Expected to find %s forms but got %s for entry \"%s\" in \"%s\" context"
               nplurals
               (length msgstr)
               msgid_plural
               msgctxt))
       ((and (not msgid_plural)
             (/= 1 (length msgstr)))
        (error "Translation string range error: Extected 1 msgstr definitions associated with \"%s\" in \"%s\" context, found %s"
               msgid
               msgctxt
               (length msgstr)))))))

;; The translation table is
;; {"msgctxt1": {"msgid": (entry), "msgid2": (entry2)}}
(defun gettext-parser--po-normalize (parser tokens)
  "Compose the result table from TOKENS.
PARSER is the parser object."
  (let ((table (make-hash-table))
        (nplurals 1)
        (msgctxt nil))
    (puthash 'headers nil table)
    (puthash 'translations (gettext-parser--translation-table) table)
    (dolist (node tokens)
      (catch 'continue
        (when (oref node obsolete)
          (unless (-> (map-elt table 'obsolete)
                      (map-elt msgctxt))
            (setf (-> (map-elt table 'obsolete)
                      (map-elt msgctxt))
                  (make-hash-table)))
          (oset node obsolete nil)
          (setf (-> (map-elt table 'obsolete)
                    (map-elt msgctxt)
                    (map-elt (oref node msgid)))
                node)
          (throw 'continue nil))
        (unless (gettext-parser--translation-table--elt
                 (map-elt table 'translations)
                 msgctxt)
          (gettext-parser--translation-table--put
           (map-elt table 'translations)
           msgctxt (make-hash-table)))
        (when (and (not (map-elt table 'headers))
                   (not msgctxt)
                   (not (oref node msgid)))
          (setf (map-elt table 'headers)
                (gettext-parser--parse-header
                 (-> (oref node msgstr)
                     (elt 0))))
          (setq nplurals
                (gettext-parser--parse-nplural-from-header-safely
                 (map-elt table 'headers)
                 nplurals)))
        (gettext-parser--po-validate-token
         parser
         node
         (map-elt table 'translations)
         msgctxt
         nplurals)))
    table))

(defun gettext-parser--po-finalize (parser tokens)
  "Convert parsed TOKENS to a translation table.
PARSER is the parser object."
  (let ((data (gettext-parser--po-join-string-values tokens)))
    (setq data (gettext-parser--po-parse-comments data))
    (setq data (gettext-parser--po-handle-keys data))
    (setq data (gettext-parser--po-handle-values parser data))
    (gettext-parser--po-normalize parser data)))

;; (defun gettext-parser-po-compile ())
;; (defun gettext-parser-mo-parse ())
;; (defun gettext-parser-mo-compile ())

(provide 'gettext-parser)

;;; gettext-parser.el ends here
