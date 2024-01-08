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

(require 'cl-lib)

;;;; Data

(cl-defstruct (kisaragi-translate--project
               (:copier nil)
               (:constructor kisaragi-translate--project))
  "Represent a project.
NAME: the name of the project.
PATH: the project contains translation files within PATH."
  name path)
(cl-defstruct (kisaragi-translate--entry
               (:copier nil)
               (:constructor kisaragi-translate--entry))
  source target)

;;;; UI

(require 'bufview)

;;;;; The editable region workaround.
;; Given a UI like this, in a non-read-only buffer:
;;
;;   Some read only UI heading text:
;;   User editing area
;;
;;   Another read only UI heading text
;;
;; We want "User editing area" to always be editable, even if the whole string
;; has been deleted. But we also don't want its preceding newline character to
;; be able to be deleted. This achieves that.
(defun kisaragi-translate--make-region-editable (start end)
  "Ensure that the region between START and END is editable."
  (put-text-property start end 'read-only nil)
  (put-text-property start end 'rear-nonsticky nil)
  (put-text-property start end 'front-sticky nil))
(defvar-local kisaragi-translate--editable-positions nil
  "Buffer position markers that must remain editable.
Instead of modifying this directly, try using
`kisaragi-translate--insert-edit-area'.")
(defvar-local kisaragi-translate--should-reset-read-only nil
  "Used for the editable workaround.
When resetting `inhibit-read-only' to the default value,
`inhibit-read-only' might've been set by a let-bind block and not
by us. This allows marking that it *is* set by us and is safe to
reset.")
(defvar-local kisaragi-translate--pre-self-insert-point nil
  "Used for the editable workaround.
The post-command hook sets the inserted text to be editable. That
region may not be just one character in size, because
`self-insert-command' can insert multiple characters at once.
This variable is used to remember where the region starts.")
(defun kisaragi-translate--editable-workaround--precmdh ()
  "The `pre-command-hook' for the editable workaround."
  (when (and (get-text-property (point) 'read-only)
             (eq this-command #'self-insert-command)
             (cl-some (lambda (mk)
                        (= mk (point)))
                      kisaragi-translate--editable-positions))
    ;; Store the point before insertion. The inserted text is not
    ;; necessary just one character as `self-insert-command' has a
    ;; prefix argument to insert multiple characters at once.
    (setq kisaragi-translate--pre-self-insert-point (point)
          kisaragi-translate--should-reset-read-only t
          inhibit-read-only t)))
(defun kisaragi-translate--editable-workaround--postcmdh ()
  "The `post-command-hook' for the editable workaround."
  ;; We've just inserted read-only text. Reset its read-only
  ;; status.
  (when (and (get-text-property (point) 'read-only)
             (eq this-command #'self-insert-command))
    (let ((inhibit-read-only t)
          (start kisaragi-translate--pre-self-insert-point)
          (end (point)))
      (when (and start
                 end
                 (< start end))
        (kisaragi-translate--make-region-editable start end))))
  (when kisaragi-translate--should-reset-read-only
    (setq inhibit-read-only nil
          kisaragi-translate--should-reset-read-only nil
          kisaragi-translate--pre-self-insert-point nil)))
(define-derived-mode kisaragi-translate--base-mode text-mode
  "Kisaragi Translate"
  "The basis major mode for all Kisaragi Translate major modes."
  (add-hook 'pre-command-hook #'kisaragi-translate--editable-workaround--precmdh nil t)
  ;; We use `post-command-hook', not `post-self-insert-hook', to be extra sure
  ;; we always reset `inhibit-read-only'.
  (add-hook 'post-command-hook #'kisaragi-translate--editable-workaround--precmdh nil t))

;;;;; Wrapper
(defmacro kisaragi-translate--define-view (name arglist docstring &rest body)
  "Wrapper around `bufview-define'."
  (declare (doc-string 3) (indent 2))
  (let* ((view (intern (format "kisaragi-translate--%s-view" name)))
         (parent-mode 'kisaragi-translate--base-mode))
    `(bufview-define ,view ,parent-mode ,arglist
       ,docstring
       ,@body)))

;;;;; Widgets
(defun kisaragi-translate--insert-edit-area (initial-input)
  "Insert an edit area, with INITIAL-INPUT as the content.
The area is made sure to be editable."
  (push (point-marker) kisaragi-translate--editable-positions)
  (let (start end)
    (setq start (point))
    (insert initial-input)
    (setq end (point))
    (kisaragi-translate--make-region-editable start end))
  (kisaragi-translate--insert-ui-text "\n" 'default
    :edit-before t))
;; Initially learned from `suggest--insert-heading'.
(cl-defun kisaragi-translate--insert-ui-text
    (text face &key (edit-before nil) (edit-after nil))
  "Insert TEXT as an UI element.
TEXT will be displayed with FACE.
If EDIT-BEFORE or EDIT-AFTER is non-nil, allow editing
immediately before or immediately after the inserted string."
  (declare (indent 2))
  (let* (start end)
    (setq start (point))
    (insert
     (propertize text
                 'read-only t
                 'front-sticky (not edit-before)
                 'rear-nonsticky edit-after))
    (setq end (point))
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'face face))))

;;;;; Views
(kisaragi-translate--define-view entry (entry)
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
  (bufview-local-set :entry entry)
  :revert
  (kisaragi-translate--insert-ui-text "Source\n" 'bold)
  (kisaragi-translate--insert-ui-text
      ;; Test string
      (concat (oref (bufview-local-get :entry) source)
              "\n")
      'font-lock-constant-face)
  (kisaragi-translate--insert-ui-text "\n" 'default)
  (kisaragi-translate--insert-ui-text "Target\n" 'bold
    :edit-after t)
  (kisaragi-translate--insert-edit-area
   (oref (bufview-local-get :entry) target))
  (kisaragi-translate--insert-ui-text "\n" 'default)
  (kisaragi-translate--insert-ui-text "Glossary" 'bold))
(kisaragi-translate--define-view entry-list ()
  "View for listing entries in a file.
Alternatively, a `completing-read'-based command is also provided
for selecting entries of a file."
  :init (read-only-mode)
  :revert)
(kisaragi-translate--define-view file-list ()
  "View for listing files in a project.
Alternatively, a `completing-read'-based command is also provided
for selecting files of a project.")

(kisaragi-translate--entry-view
 (kisaragi-translate--entry
  :source
  (concat "Use KDE software to surf the web, keep in touch with colleagues, friends and "
          "family, manage your files, enjoy music and videos; and get creative and "
          "productive at work. The KDE community develops and maintains more than "
          "<strong>200</strong> applications which run on any Linux desktop, and often "
          "other platforms too.")
  :target "KDEのソフトウェアでウェブに"))

(provide 'kisaragi-translate)
;;; kisaragi-translate.el ends here
