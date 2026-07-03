;;; eglot-roslyn-plan-test.el --- Tests for my/eglot-roslyn-workspace-plan -*- lexical-binding: t; -*-

;; `my/eglot-roslyn-workspace-plan' is the pure decision behind the Roslyn
;; workspace-open notification: given a project ROOT it returns a plist saying
;; whether to send `:solution/open' (nearest top-level .sln) or `:project/open'
;; (all .csproj found recursively), or nil. It inspects the filesystem only,
;; leaving the eglot/jsonrpc call to its wrapper.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun erp-test--make-root ()
  "Create and return a fresh temp project ROOT directory (with trailing slash)."
  (file-name-as-directory (make-temp-file "erp-root" t)))

(defun erp-test--touch (root relpath)
  "Create an empty file at RELPATH under ROOT, making parent dirs as needed."
  (let ((file (expand-file-name relpath root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert ""))
    file))

(ert-deftest eglot-roslyn-plan/sln-selects-solution-open ()
  "A top-level .sln yields a :solution/open plan pointing at that file."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-roslyn-workspace-plan)
  (let ((root (erp-test--make-root)))
    (unwind-protect
        (let ((sln (erp-test--touch root "App.sln")))
          ;; Act
          (let ((plan (my/eglot-roslyn-workspace-plan root)))
            ;; Assert
            (should (eq (plist-get plan :method) :solution/open))
            (should (equal (plist-get plan :path) sln))))
      (delete-directory root t))))

(ert-deftest eglot-roslyn-plan/sln-wins-over-csproj ()
  "When both a .sln and .csproj exist, the .sln takes precedence."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-roslyn-workspace-plan)
  (let ((root (erp-test--make-root)))
    (unwind-protect
        (progn
          (erp-test--touch root "App.sln")
          (erp-test--touch root "src/Api/Api.csproj")
          ;; Act
          (let ((plan (my/eglot-roslyn-workspace-plan root)))
            ;; Assert
            (should (eq (plist-get plan :method) :solution/open))))
      (delete-directory root t))))

(ert-deftest eglot-roslyn-plan/csproj-fallback-is-recursive ()
  "With no .sln, all nested .csproj files drive a :project/open plan."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-roslyn-workspace-plan)
  (let ((root (erp-test--make-root)))
    (unwind-protect
        (let ((a (erp-test--touch root "src/Api/Api.csproj"))
              (b (erp-test--touch root "src/Core/Core.csproj")))
          ;; Act
          (let ((plan (my/eglot-roslyn-workspace-plan root)))
            ;; Assert
            (should (eq (plist-get plan :method) :project/open))
            (should (equal (sort (copy-sequence (plist-get plan :paths)) #'string<)
                           (sort (list a b) #'string<)))))
      (delete-directory root t))))

(ert-deftest eglot-roslyn-plan/empty-root-is-nil ()
  "A project with neither .sln nor .csproj yields no plan."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-roslyn-workspace-plan)
  (let ((root (erp-test--make-root)))
    (unwind-protect
        ;; Act
        (let ((plan (my/eglot-roslyn-workspace-plan root)))
          ;; Assert
          (should-not plan))
      (delete-directory root t))))

(provide 'eglot-roslyn-plan-test)
;;; eglot-roslyn-plan-test.el ends here
