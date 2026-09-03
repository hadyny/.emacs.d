;;; flycheck-margin-items-test.el --- Tests for my/flycheck-margin-items -*- lexical-binding: t; -*-

;; `my/flycheck-margin-items' is the `svg-margin' provider function registered
;; in the Diagnostics (Flycheck) section of config.org: it turns
;; `flycheck-current-errors' into one indicator plist per error.  Two of its
;; keys have a real behavioural contract, not just a shape:
;;
;;   * `:action' is a closure over that error's position, wrapped in
;;     `(interactive)' -- `svg-margin' (like `svg-line''s tab-bar wiring) can
;;     run it via `call-interactively', so it must be a genuine command;
;;   * `:menu' entries are plain, non-interactive functions -- `svg-margin''s
;;     popup path (like `svg-line''s) `funcall's a non-command choice, so no
;;     `(interactive)' is needed there.
;;
;; `flycheck-error-line'/`-level'/`-message'/`-pos' are stubbed rather than
;; built from a real `flycheck-error' struct: Flycheck is not on the local,
;; non-Nix load-path this test suite runs against (see flycheck-test.el), and
;; the function only ever calls those four accessors, so stubbing them keeps
;; this test independent of whether the real package is installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defmacro flycheck-margin-items-test--with-stub-errors (errors &rest body)
  "Run BODY in a temp buffer with `flycheck-mode' on and ERRORS as its errors.
ERRORS is a list of (LINE LEVEL MESSAGE POS) lists; the four `flycheck-error-*'
accessors are stubbed to read off of one of those lists directly, so an
\"error\" here is just its own plist rather than a real `flycheck-error' struct."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local flycheck-mode t)
     (setq-local flycheck-current-errors ,errors)
     (cl-letf (((symbol-function 'flycheck-error-line) (lambda (e) (nth 0 e)))
               ((symbol-function 'flycheck-error-level) (lambda (e) (nth 1 e)))
               ((symbol-function 'flycheck-error-message) (lambda (e) (nth 2 e)))
               ((symbol-function 'flycheck-error-pos) (lambda (e) (nth 3 e))))
       ,@body)))

(ert-deftest flycheck-margin-items/nil-outside-flycheck-mode ()
  "Returns nil when `flycheck-mode' is off, even with stale errors around."
  ;; Arrange
  (cfg-test-load-defun 'my/flycheck-margin-items)
  (with-temp-buffer
    (setq-local flycheck-mode nil)
    (setq-local flycheck-current-errors '((1 error "stale" 1)))
    ;; Act / Assert
    (should-not (my/flycheck-margin-items (current-buffer)))))

(ert-deftest flycheck-margin-items/one-indicator-per-error-with-level-face ()
  "One indicator per error, at its line, faced/prioritised by level."
  ;; Arrange
  (cfg-test-load-defun 'my/flycheck-margin-items)
  ;; Act
  (flycheck-margin-items-test--with-stub-errors
      '((3 error "boom" 10) (7 warning "meh" 40) (9 info "fyi" 60))
    (let ((items (my/flycheck-margin-items (current-buffer))))
      ;; Assert
      (should (= (length items) 3))
      (should (equal (mapcar (lambda (i) (plist-get i :line)) items) '(3 7 9)))
      (should (eq (plist-get (nth 0 items) :face) 'flycheck-fringe-error))
      (should (eq (plist-get (nth 1 items) :face) 'flycheck-fringe-warning))
      (should (eq (plist-get (nth 2 items) :face) 'flycheck-fringe-info))
      (should (> (plist-get (nth 0 items) :priority) (plist-get (nth 1 items) :priority)))
      (should (> (plist-get (nth 1 items) :priority) (plist-get (nth 2 items) :priority)))
      (should (equal (plist-get (nth 0 items) :help) "boom")))))

(ert-deftest flycheck-margin-items/action-is-an-interactive-jump ()
  "`:action' is a genuine command (required by `call-interactively') that
moves point to that error's position."
  ;; Arrange
  (cfg-test-load-defun 'my/flycheck-margin-items)
  ;; Act
  (flycheck-margin-items-test--with-stub-errors '((5 error "boom" 42))
    (insert (make-string 100 ?x))
    (let* ((action (plist-get (car (my/flycheck-margin-items (current-buffer))) :action)))
      ;; Assert -- a real interactive command, not a plain closure.
      (should (commandp action))
      (goto-char (point-min))
      (call-interactively action)
      (should (= (point) 42)))))

(ert-deftest flycheck-margin-items/menu-lists-all-errors-non-interactively ()
  "`:menu' offers `flycheck-list-errors', funcalled by `svg-margin''s popup."
  ;; Arrange
  (cfg-test-load-defun 'my/flycheck-margin-items)
  ;; Act
  (flycheck-margin-items-test--with-stub-errors '((5 error "boom" 42))
    (let* ((menu (plist-get (car (my/flycheck-margin-items (current-buffer))) :menu)))
      ;; Assert
      (should (equal (cdr (assoc "List all errors" menu)) #'flycheck-list-errors)))))

(provide 'flycheck-margin-items-test)
;;; flycheck-margin-items-test.el ends here
