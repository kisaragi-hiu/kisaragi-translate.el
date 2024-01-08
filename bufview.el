;;; bufview.el --- A buffer-based view system -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(defun bufview--split-body-keywords (body)
  "Split BODY into parts determined by keywords.
Implements similar logic to `use-package'\\='s body.

   (:init form another-form
    :revert form another-form)
-> ((:init . (form another-form))
    (:revert . (form another-form)))"
  (let* ((keywords '(:init :revert))
         (body-table (make-hash-table))
         (current nil))
    (dolist (elem body)
      (if (memq elem keywords)
          (setq current elem)
        (unless current
          (error "There should not be forms before the first keyword"))
        (push elem (gethash current body-table))))
    (cl-loop
     for keyword in keywords
     collect (cons keyword
                   (nreverse (gethash keyword body-table))))))

;; TODO: buffer local state system
(defmacro bufview-define (name parent-mode arglist docstring &rest body)
  "Define a view called NAME, described by DOCSTRING.

A view consists of:

- an init command, for setting up the buffer, is the entry point of the view
  (NAME, ARGLIST, and DOCSTRING are for this);
- a revert command, for rendering the buffer contents;
- and a major mode (called `NAME--major-mode', derived from PARENT-MODE).

The bodies of the init command and the revert command are both
specified in BODY, with keywords, similar to `use-package'. For instance:

\(bufview-define my-view view-mode ()
  \"My view.\"
  :init
  (rainbow-delimiters-mode)
  :revert
  (insert \"Hello!\"))

The init command opens a buffer, runs INIT-BODY in it, then displays it.
The revert command clears the buffer content, then runs REVERT-BODY.

`interactive' and `declare' should work as usual in BODY, and
apply to the init command. If no interactive form is provided,
the init function is still made interactive regardless. If there
is more than zero required arguments, use the interactive form to
declare what they should be."
  (declare (doc-string 4) (indent 3))
  (let* ((child (intern (format "%s--major-mode" name)))
         (human-name (capitalize
                      (replace-regexp-in-string
                       "-+" " "
                       (format "%s" name))))
         (mode-name human-name)
         (mode-doc (format "Major mode for the %s view."
                           human-name))
         (init-name name)
         (revert-name (intern (format "%s--revert" name)))
         (split (bufview--split-body-keywords body))
         (init-body (alist-get :init split))
         (revert-body (alist-get :revert split))
         (buffer (format "*%s*" human-name))
         ;; Grab the declare and interactive forms like `defun' does.
         (parse (byte-run--parse-body init-body t))
         (init-decl-form (nth 1 parse))
         (init-intv-form (nth 2 parse))
         (init-body (nth 3 parse)))
    (cl-with-gensyms (buf)
      `(progn
         (define-derived-mode ,child ,parent-mode
           ,mode-name
           ,mode-doc
           :interactive nil
           (setq-local revert-buffer-function #',revert-name))
         (defun ,revert-name (_ignore-auto _no-confirm)
           ,(format "Revert the %s buffer." human-name)
           (let ((inhibit-read-only t))
             (erase-buffer)
             ,@revert-body))
         (defun ,init-name ,arglist
           ,docstring
           ,@(when init-decl-form
               `(,init-decl-form))
           ;; The init command should always be interactive
           ,(or init-intv-form
                '(interactive))
           (let ((,buf (get-buffer-create ,buffer)))
             (if (derived-mode-p 'kisaragi-translation-mode)
                 (pop-to-buffer-same-window ,buf)
               (pop-to-buffer ,buf))
             (with-current-buffer ,buf
               (,child)
               (progn
                 ,@(when init-body
                     `((let ((inhibit-read-only t))
                         ,@init-body)))
                 (revert-buffer)
                 (goto-char (point-min))))))))))

(provide 'bufview)

;;; bufview.el ends here
