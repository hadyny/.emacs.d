;;; catppuccin-flavor-test.el --- Tests for my/catppuccin-flavor-for -*- lexical-binding: t; -*-

;; `my/catppuccin-flavor-for' maps a system appearance (`light'/`dark') to the
;; catppuccin flavor symbol the whole theme switch is built on. Its docstring
;; promises it is pure and unit-testable without loading a theme; these tests
;; hold it to that.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest catppuccin-flavor/light-is-latte ()
  "A light appearance selects the Latte flavor."
  ;; Arrange
  (cfg-test-load-defun 'my/catppuccin-flavor-for)
  ;; Act
  (let ((flavor (my/catppuccin-flavor-for 'light)))
    ;; Assert
    (should (eq flavor 'latte))))

(ert-deftest catppuccin-flavor/dark-is-mocha ()
  "A dark appearance selects the Mocha flavor."
  ;; Arrange
  (cfg-test-load-defun 'my/catppuccin-flavor-for)
  ;; Act
  (let ((flavor (my/catppuccin-flavor-for 'dark)))
    ;; Assert
    (should (eq flavor 'mocha))))

(ert-deftest catppuccin-flavor/unknown-defaults-to-mocha ()
  "Anything other than `light' falls back to the dark Mocha flavor."
  ;; Arrange
  (cfg-test-load-defun 'my/catppuccin-flavor-for)
  ;; Act
  (let ((flavor (my/catppuccin-flavor-for nil)))
    ;; Assert
    (should (eq flavor 'mocha))))

(provide 'catppuccin-flavor-test)
;;; catppuccin-flavor-test.el ends here
