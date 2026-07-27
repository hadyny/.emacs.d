;;; dired-find-file-reuse-test.el --- Tests for my/dired-find-file-reuse -*- lexical-binding: t; -*-

;; `my/dired-find-file-reuse' is bound to RET in dired to work around the
;; `(< (length (get-buffer-window-list)) 2)' guard in
;; `dired--find-possibly-alternative-file': it reuses the current dired buffer
;; for *directories* unconditionally (via `find-alternate-file'), while opening
;; *files* normally (via `find-file').  These tests pin that dispatch, stubbing
;; the visit/open primitives so no real files or buffers are touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest dired-find-file-reuse/directory-reuses-buffer ()
  "Selecting a directory reuses the buffer via `find-alternate-file'."
  ;; Arrange
  (cfg-test-load-defun 'my/dired-find-file-reuse)
  (let ((alternate nil) (normal nil))
    (cl-letf (((symbol-function 'dired-get-file-for-visit) (lambda () "/some/dir"))
              ((symbol-function 'file-directory-p) (lambda (_f) t))
              ((symbol-function 'find-alternate-file) (lambda (f) (setq alternate f)))
              ((symbol-function 'find-file) (lambda (f) (setq normal f))))
      ;; Act
      (my/dired-find-file-reuse)
      ;; Assert
      (should (equal alternate "/some/dir"))
      (should-not normal))))

(ert-deftest dired-find-file-reuse/file-opens-normally ()
  "Selecting a regular file opens it with `find-file', not in place."
  ;; Arrange
  (cfg-test-load-defun 'my/dired-find-file-reuse)
  (let ((alternate nil) (normal nil))
    (cl-letf (((symbol-function 'dired-get-file-for-visit) (lambda () "/some/file.txt"))
              ((symbol-function 'file-directory-p) (lambda (_f) nil))
              ((symbol-function 'find-alternate-file) (lambda (f) (setq alternate f)))
              ((symbol-function 'find-file) (lambda (f) (setq normal f))))
      ;; Act
      (my/dired-find-file-reuse)
      ;; Assert
      (should (equal normal "/some/file.txt"))
      (should-not alternate))))

(provide 'dired-find-file-reuse-test)
;;; dired-find-file-reuse-test.el ends here
