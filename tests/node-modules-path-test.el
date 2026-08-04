;;; node-modules-path-test.el --- Tests for my/add-node-modules-path -*- lexical-binding: t; -*-

;; `my/add-node-modules-path' locates the nearest node_modules/.bin via
;; `locate-dominating-file' and prepends it to a *buffer-local* `exec-path' and
;; PATH. It replaces `add-node-modules-path', whose `npm bin' default broke in
;; npm 9. These tests pin the three branches and the buffer-local scoping.
;;
;; The tests also pin the hooks that run the function.  `js-mode-hook' alone was
;; a defect.  The configuration calls `treesit-auto-add-to-auto-mode-alist' with
;; `all', so a .js file opens in `js-ts-mode'.  Emacs does not run
;; `js-mode-hook' in a `js-ts-mode' buffer.  Both modes derive from
;; `js-base-mode', and `provided-mode-derived-p' does report `js-ts-mode' ->
;; `js-mode', but that relation controls `:modes' matching only.  The function
;; therefore never ran for a .js file, and the project-local `eslint' and
;; `typescript-language-server' stayed off the PATH.  `js-mode-hook' stays as
;; well, because `treesit-auto' falls back to `js-mode' if the JavaScript
;; grammar is absent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defconst nmp-test--required-hooks
  '(tsx-ts-mode-hook typescript-ts-mode-hook js-ts-mode-hook js-mode-hook)
  "The hooks that must run `my/add-node-modules-path'.")

(defun nmp-test--config-loaded-p ()
  "Return non-nil if this Emacs has loaded the full configuration."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

(defun nmp-test--hooks ()
  "Return the hooks that config.el attaches `my/add-node-modules-path' to."
  (let (hooks)
    (dolist (form (cfg-test-read-forms))
      ;; Shape one: (dolist (hook '(HOOK ...)) (add-hook hook #'FN))
      (dolist (loop (cfg-test-find-all form 'dolist))
        (when (string-match-p "my/add-node-modules-path" (prin1-to-string loop))
          (let ((value (nth 1 (nth 1 loop))))
            (when (eq (car-safe value) 'quote)
              (setq hooks (append hooks (cadr value)))))))
      ;; Shape two: (add-hook 'HOOK #'FN)
      (dolist (call (cfg-test-find-all form 'add-hook))
        (when (equal (nth 2 call) '(function my/add-node-modules-path))
          (let ((hook (nth 1 call)))
            (when (eq (car-safe hook) 'quote)
              (push (cadr hook) hooks))))))
    (delete-dups hooks)))

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

;;; Hook wiring -- which modes actually run the function

(ert-deftest node-modules-path/runs-on-the-tree-sitter-js-hook ()
  "config.el attaches the function to `js-ts-mode-hook'.
A .js file opens in `js-ts-mode', and `js-mode-hook' does not run there."
  ;; Arrange / Act
  (let ((hooks (nmp-test--hooks)))
    ;; Assert
    (should (memq 'js-ts-mode-hook hooks))))

(ert-deftest node-modules-path/covers-every-js-and-ts-mode ()
  "The function runs for TSX, TypeScript and JavaScript, in both mode families.
`js-mode-hook' stays for the fallback.  `treesit-auto' uses `js-mode' if the
JavaScript grammar is absent."
  ;; Arrange / Act
  (let ((hooks (nmp-test--hooks)))
    ;; Assert
    (dolist (hook nmp-test--required-hooks)
      (should (memq hook hooks)))))

(ert-deftest node-modules-path/hooks-are-live-after-load ()
  "Emacs runs the function from each hook once the configuration is loaded."
  ;; Arrange
  (skip-unless (nmp-test--config-loaded-p))
  ;; Assert
  (dolist (hook nmp-test--required-hooks)
    (should (memq 'my/add-node-modules-path (symbol-value hook)))))

(ert-deftest node-modules-path/js-mode-hook-does-not-run-in-js-ts-mode ()
  "Emacs does not run `js-mode-hook' in a `js-ts-mode' buffer.
This is the reason for the `js-ts-mode-hook' entry.  Do not remove that entry
and keep `js-mode-hook' alone."
  ;; Arrange
  (should (require 'js nil t))
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'javascript)))
  (let (fired)
    (let ((js-mode-hook (list (lambda () (push 'js-mode-hook fired))))
          (js-ts-mode-hook (list (lambda () (push 'js-ts-mode fired)))))
      ;; Act
      (with-temp-buffer
        (js-ts-mode))
      ;; Assert
      (should (memq 'js-ts-mode fired))
      (should-not (memq 'js-mode-hook fired)))))

(provide 'node-modules-path-test)
;;; node-modules-path-test.el ends here
