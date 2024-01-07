;;; kisaragi-translate.el --- A Lokalize equivalent -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Version: 0.0.1
;; Keywords: i18n apps
;; Homepage: https://github.com/kisaragi-hiu/kisaragi-translate
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Might become a Lokalize equivalent in Emacs.
;;
;; When I use present tense in the descriptions keep in mind that is more like
;; the spec, and does not necessarily describe how things currently are.
;;
;; Translation is written as *entries*, which live inside *files*, which live
;; inside *projects*.
;;
;;; Code:

(define-derived-mode kisaragi-translate-mode text-mode
  "Kisaragi Translate"
  "Root major mode for all Kisaragi Translate major modes.")
(defmacro kisaragi-translate--define-view (name arglist docstring &rest body)
  "Define a view called NAME, described by DOCSTRING.

A view consists of a command (defined using NAME (see below),
ARGLIST, and DOCSTRING, see `defun') and a major mode. The
command opens a buffer, runs INIT-BODY to fill in the contents,
then displays it.

For convenience, NAME should be a partial name.
For example, if NAME is \"entry\",
then the major mode is \"kisaragi-translate--entry-mode\",
and the command is \"kisaragi-translate-entry-view\".

`interactive' and `declare' should work as usual in BODY, except
that if no interactive form is provided, the init function is
still made interactive regardless. If there is more than zero
required arguments, use the interactive form to declare what they
should be."
  (declare (doc-string 3) (indent 2))
  (let* ((child (intern (format "kisaragi-translate--%s-mode" name)))
         (parent 'kisaragi-translate-mode)
         (mode-name (format "Kisaragi Translate %s" name))
         (mode-doc (format "Major mode for the Kisaragi Translate %s view."
                           (capitalize (format "%s" name))))
         (init-name (intern (format "kisaragi-translate-%s-view" name)))
         (buffer (format "*Kisaragi Translate %s*" name))
         ;; Grab the declare and interactive forms like `defun' does.
         (parse (byte-run--parse-body body t))
         (decl-form (nth 1 parse))
         (intv-form (nth 2 parse))
         (body (nth 3 parse)))
    (cl-with-gensyms (buf)
      `(progn
         (define-derived-mode ,child ,parent
           ,mode-name
           ,mode-doc
           :interactive nil)
         (defun ,init-name ,arglist
           ,docstring
           ,@(when decl-form
               `(,decl-form))
           ;; The command should always be interactive
           ,(or intv-form
                '(interactive))
           (let ((,buf (get-buffer-create ,buffer)))
             (if (derived-mode-p 'kisaragi-translation-mode)
                 (pop-to-buffer-same-window ,buf)
               (pop-to-buffer ,buf))
             (with-current-buffer ,buf
               (,child)
               (prog1 ,@(when body
                          `((let ((inhibit-read-only t))
                              ,@body)))
                 (goto-char (point-min))))))))))

(kisaragi-translate--define-view entry ()
  "View for editing an entry.
Shows the source text, a place for editing the target text, as
well as references such as Translation Memory and Glossary.
Commands are provided to switch to the next or previous entries.")
(kisaragi-translate--define-view entry-list ()
  "View for listing entries in a file.
Alternatively, a `completing-read'-based command is also provided
for selecting entries of a file.")
(kisaragi-translate--define-view file-list ()
  "View for listing files in a project.
Alternatively, a `completing-read'-based command is also provided
for selecting files of a project.")

(provide 'kisaragi-translate)
;;; kisaragi-translate.el ends here
