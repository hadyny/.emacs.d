;;; magit-todos-test.el --- Tests for the magit-todos setup -*- lexical-binding: t; -*-

;; Specifies the magit-todos install:
;;
;; * magit-todos is configured via use-package, loaded `:after magit', and
;;   turns `magit-todos-mode' on -- the global mode is what pushes the TODOs
;;   section onto `magit-status-sections-hook', so without it the section never
;;   appears (structural: parses the tangled config.el, runs anywhere);
;;
;; * the standalone list buffer is reachable from `<leader> g t' (structural);
;;
;; * both entry points are autoloadable from the Nix package set (behavioural).
;;   It deliberately does not `require' magit-todos: loading Magit shells out to
;;   `git', which is absent in the Nix build sandbox (see keybindings-test.el),
;;   so this checks the autoload cookies only and self-skips on a bare
;;   `emacs-nox' run.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun magit-todos-test--use-package (name)
  "Return the `use-package' form for NAME in config.el, or nil if absent."
  (cl-loop for form in (cfg-test-read-forms)
           thereis (cl-loop for up in (cfg-test-find-all form 'use-package)
                            when (eq (nth 1 up) name) return up)))

(defun magit-todos-test--after-features (form)
  "Return the features listed by FORM's `:after' keyword, as a list."
  (let ((after (plist-get (nthcdr 2 form) :after)))
    (cond ((null after) nil)
          ((symbolp after) (list after))
          ((consp after) after))))

(ert-deftest magit-todos/configured ()
  "magit-todos is configured via use-package."
  ;; Arrange / Act
  (let ((form (magit-todos-test--use-package 'magit-todos)))
    ;; Assert
    (should form)))

(ert-deftest magit-todos/loads-after-magit ()
  "magit-todos loads with magit, so its section is registered before a refresh."
  ;; Arrange / Act
  (let ((form (magit-todos-test--use-package 'magit-todos)))
    ;; Assert
    (should form)
    (should (memq 'magit (magit-todos-test--after-features form)))))

(ert-deftest magit-todos/mode-enabled ()
  "The config turns `magit-todos-mode' on, which inserts the TODOs section."
  ;; Arrange / Act
  (let ((form (magit-todos-test--use-package 'magit-todos)))
    ;; Assert
    (should form)
    (should (cfg-test-find-all form 'magit-todos-mode))))

(ert-deftest magit-todos/list-bound ()
  "`magit-todos-list' is bound in the Magit leader namespace."
  ;; Arrange
  (let (bindings)
    ;; Act
    (dolist (form (cfg-test-read-forms))
      (dolist (edk (cfg-test-find-all form 'evil-define-key))
        (let ((pairs (nthcdr 3 edk)))
          (while (cdr pairs)
            (push (cons (car pairs) (cadr pairs)) bindings)
            (setq pairs (cddr pairs))))))
    ;; Assert
    (should (member (cons '(kbd "<leader> g t") ''magit-todos-list) bindings))))

(ert-deftest magit-todos/entry-points-autoloaded ()
  "The mode and the list command are autoloadable from the Nix package set."
  ;; Arrange: keyed off the autoloaded `magit-status' rather than `featurep',
  ;; since nothing here is loaded in batch -- it only marks that the real
  ;; package set is present (absent on a bare emacs-nox run).
  (skip-unless (fboundp 'magit-status))
  ;; Assert
  (should (fboundp 'magit-todos-mode))
  (should (fboundp 'magit-todos-list)))

(provide 'magit-todos-test)
;;; magit-todos-test.el ends here
