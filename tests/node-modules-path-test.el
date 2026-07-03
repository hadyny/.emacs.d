;;; node-modules-path-test.el --- Tests for my/add-node-modules-path -*- lexical-binding: t; -*-

;; `my/add-node-modules-path' locates the nearest node_modules/.bin via
;; `locate-dominating-file' and prepends it to a *buffer-local* `exec-path' and
;; PATH. It replaces `add-node-modules-path', whose `npm bin' default broke in
;; npm 9. These tests pin the three branches and the buffer-local scoping.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun nmp-test--make-tree (with-bin)
  "Create a temp project tree and return (ROOT SUBDIR BIN).
When WITH-BIN is non-nil, create the node_modules/.bin directory; otherwise
create only node_modules so the .bin guard is exercised."
  (let* ((root (file-name-as-directory (make-temp-file "nmp-root" t)))
         (subdir (file-name-as-directory (expand-file-name "src/app" root)))
         (bin (file-name-as-directory (expand-file-name "node_modules/.bin" root))))
    (make-directory subdir t)
    (if with-bin
        (make-directory bin t)
      (make-directory (expand-file-name "node_modules" root) t))
    (list root subdir bin)))

(ert-deftest node-modules-path/prepends-bin-when-present ()
  "When node_modules/.bin exists up the tree, it is prepended to exec-path/PATH."
  ;; Arrange
  (cfg-test-load-defun 'my/add-node-modules-path)
  (cl-destructuring-bind (root subdir bin) (nmp-test--make-tree t)
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory subdir))
            ;; Act
            (my/add-node-modules-path)
            ;; Assert
            (should (equal (car exec-path) (directory-file-name bin)))
            (should (string-prefix-p (concat "PATH=" (directory-file-name bin))
                                     (car process-environment)))))
      (delete-directory root t))))

(ert-deftest node-modules-path/mutation-is-buffer-local ()
  "The exec-path change must not leak into the global value."
  ;; Arrange
  (cfg-test-load-defun 'my/add-node-modules-path)
  (cl-destructuring-bind (root subdir _bin) (nmp-test--make-tree t)
    (let ((global-exec-path (default-value 'exec-path)))
      (unwind-protect
          (progn
            ;; Act
            (with-temp-buffer
              (let ((default-directory subdir))
                (my/add-node-modules-path)
                (should (local-variable-p 'exec-path))))
            ;; Assert: global value is untouched after the buffer is gone
            (should (equal (default-value 'exec-path) global-exec-path)))
        (delete-directory root t)))))

(ert-deftest node-modules-path/no-change-when-bin-missing ()
  "node_modules present but no .bin directory leaves exec-path untouched."
  ;; Arrange
  (cfg-test-load-defun 'my/add-node-modules-path)
  (cl-destructuring-bind (root subdir _bin) (nmp-test--make-tree nil)
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory subdir)
                (before exec-path))
            ;; Act
            (my/add-node-modules-path)
            ;; Assert
            (should (equal exec-path before))
            (should-not (local-variable-p 'exec-path))))
      (delete-directory root t))))

(ert-deftest node-modules-path/no-change-without-node-modules ()
  "With no node_modules anywhere up the tree, exec-path is untouched."
  ;; Arrange
  (cfg-test-load-defun 'my/add-node-modules-path)
  (let* ((root (file-name-as-directory (make-temp-file "nmp-bare" t)))
         (subdir (file-name-as-directory (expand-file-name "src/app" root))))
    (make-directory subdir t)
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory subdir)
                (before exec-path))
            ;; Act
            (my/add-node-modules-path)
            ;; Assert
            (should (equal exec-path before))
            (should-not (local-variable-p 'exec-path))))
      (delete-directory root t))))

(provide 'node-modules-path-test)
;;; node-modules-path-test.el ends here
