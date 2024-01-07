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

;; TODO: distinguish init and revert
;; init is for setting up variables, especially local state; revert is for the
;; actual rendering
;; TODO: buffer local state system
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
         (buffer (format "*Kisaragi Translate %s*"
                         (capitalize (format "%s" name))))
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
           :interactive nil
           (setq-local revert-buffer-function
                       (lambda (&rest _)
                         (,init-name))))
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
               (let ((inhibit-read-only t))
                 (erase-buffer))
               (,child)
               (prog1 ,@(when body
                          `((let ((inhibit-read-only t))
                              ,@body)))
                 (goto-char (point-min))))))))))
;; Based on `suggest--insert-heading'.
(cl-defun kisaragi-translate--insert-ui-text
    (text face &key (edit-before nil) (edit-after nil))
  "Insert TEXT as an UI element."
  ;; Note that:
  ;;
  ;;   Read only UI text|
  ;;   |user input
  ;;
  ;; The two "|" positions are treated the same. If editing after the read only
  ;; segment is allowed, then we end up allowing insertion on the same line; if
  ;; it isn't, we end up disallowing insertion at the start of the next line.
  ;; Even builtin `read-string-from-buffer' suffers from this.
  ;;
  ;; We'll need some hacks to be able to fix this behavior. Some ideas:
  ;;
  ;; - Can we [remap self-insert-command] in a major mode map? Then we can
  ;;   intercept it, setting inhibit-read-only if we're at pos-bol and the next
  ;;   position is not read-only
  ;; - Or is it possible to use a buffer local advice?
  ;; - Or some magic in `pre-command-hook' and `post-command-hook'. Locally,
  ;;   detect if we should be able to insert in `pre-command-hook' and set
  ;;   inhibit-read-only if so, then reset inhibit-read-only in
  ;;   `post-command-hook'
  ;; - Or don't use the text property, instead set the entire buffer to
  ;;   read-only then set/reset inhibit-read-only pre/post-command if point is
  ;;   between some markers
  (declare (indent 2))
  (let* ((last-index (1- (length text)))
         (excluding-last (substring text 0 last-index))
         (last-char (substring text last-index))
         start end)
    (setq start (point))
    (insert
     (propertize excluding-last
                 'read-only t
                 'front-sticky (not edit-before))
     (propertize last-char
                 'read-only t
                 'rear-nonsticky edit-after))
    (setq end (point))
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'face face))))

(kisaragi-translate--define-view entry ()
  "View for editing an entry.
Shows the source text, a place for editing the target text, as
well as references such as Translation Memory and Glossary.
Commands:
- next/prev entry
- jump to target text editing area
- jump to glossary selection area
- jump to translation memory selection area
- select glossary item
- select translation memory item
- copy source to target
- toggle fuzzy (marked with italic target text)"
  :init
  ;; init-body
  :revert
  (kisaragi-translate--insert-ui-text "Source\n" 'bold)
  (kisaragi-translate--insert-ui-text
      ;; Test string
      (concat
       "Use KDE software to surf the web, keep in touch with colleagues, friends and "
       "family, manage your files, enjoy music and videos; and get creative and "
       "productive at work. The KDE community develops and maintains more than "
       "<strong>200</strong> applications which run on any Linux desktop, and often "
       "other platforms too."
       "\n")
      'font-lock-constant-face)
  (kisaragi-translate--insert-ui-text "\n" 'default)
  (kisaragi-translate--insert-ui-text "Target\n" 'bold)
  (insert "KDEのソフトウェアでウェブに")
  (kisaragi-translate--insert-ui-text "\n\n" 'default)
  (kisaragi-translate--insert-ui-text "Glossary" 'bold))
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
