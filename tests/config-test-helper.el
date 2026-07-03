;;; config-test-helper.el --- Shared helpers for config.el unit tests -*- lexical-binding: t; -*-

;; Utilities for testing individual functions defined in the tangled
;; `config.el' without loading the whole configuration (fonts, themes, package
;; modes).  `config.el' nests its helper defuns inside `use-package' bodies, so
;; we search the read forms recursively and eval just the one under test.

;;; Code:

(require 'cl-lib)

(defun cfg-test-read-forms (&optional file)
  "Return the list of top-level forms read from FILE (default \"config.el\")."
  (with-temp-buffer
    (insert-file-contents (or file "config.el"))
    (goto-char (point-min))
    (let (forms eof)
      (while (not eof)
        (let ((f (condition-case nil
                     (read (current-buffer))
                   (end-of-file (setq eof t) nil))))
          (when f (push f forms))))
      (nreverse forms))))

(defun cfg-test-find-all (form head)
  "Return every sub-form of FORM whose car is the symbol HEAD (any depth)."
  (let (acc)
    (cl-labels ((walk (x)
                  (when (consp x)
                    (when (eq (car x) head) (push x acc))
                    (walk (car x))
                    (walk (cdr x)))))
      (walk form))
    (nreverse acc)))

(defun cfg-test-find-defun (form name)
  "Recursively search FORM for a (defun NAME ...) sub-form; return it or nil."
  (cond
   ((not (consp form)) nil)
   ((and (eq (car form) 'defun) (consp (cdr form)) (eq (cadr form) name)) form)
   ;; Walk car and cdr directly so dotted pairs (from backquote forms) are safe.
   (t (or (cfg-test-find-defun (car form) name)
          (cfg-test-find-defun (cdr form) name)))))

(defun cfg-test-load-defun (name &optional file)
  "Read FILE (default \"config.el\"), find the defun NAME at any depth, and eval it."
  (with-temp-buffer
    (insert-file-contents (or file "config.el"))
    (goto-char (point-min))
    (let (form found eof)
      (while (and (not found) (not eof))
        (setq form (condition-case nil
                       (read (current-buffer))
                     (end-of-file (setq eof t) nil)))
        (when form
          (setq found (cfg-test-find-defun form name))))
      (unless found
        (error "defun %s not found in %s" name (or file "config.el")))
      (eval found t))))

(provide 'config-test-helper)
;;; config-test-helper.el ends here
