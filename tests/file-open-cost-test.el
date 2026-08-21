;;; file-open-cost-test.el --- Tests for the cost of opening a file -*- lexical-binding: t; -*-

;; `find-file-hook' and the mode hooks run on every file a command opens, and
;; some commands (project switching, wgrep sessions, Magit) open many files in
;; a short time, so anything costly on that path is felt as lag. These tests
;; pin the two limits that keep the path cheap.
;;
;; 1. The diff-hl *global* modes are enabled once, not for each file.
;;    `global-diff-hl-mode' is a globalized minor mode, so each enable walks
;;    `buffer-list' and turns `diff-hl-mode' on in every buffer.  Enabling it
;;    from `find-file-hook' repeated that walk for each file, over a buffer list
;;    that opening many files at once makes longer.  The trigger stays on
;;    `find-file-hook' to keep the package deferred, but it must remove itself
;;    after the first file.
;;
;; 2. Flycheck does not start a check when a buffer opens.  In a TS buffer
;;    without an Eglot connection the checker is `javascript-eslint', and one run
;;    takes approximately 6 seconds.  See eglot-multiserver-test.el.
;;
;; Both tests are structural: they read the tangled config.el, so they run
;; anywhere.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun foc-test--use-package-form (name)
  "Return the `use-package' form for NAME from config.el, or nil."
  (catch 'found
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (when (eq (nth 1 up) name)
          (throw 'found up))))
    nil))

(defun foc-test--custom-value (form var)
  "Return the value that the `:custom' block of FORM gives VAR, unquoted.
Return the symbol `unset' if the block does not mention VAR."
  (let ((tail (cdr (memq :custom form)))
        (result 'unset))
    (while (and tail (not (keywordp (car tail))))
      (let ((entry (car tail)))
        (when (and (consp entry) (eq (car entry) var))
          (let ((value (cadr entry)))
            (setq result (if (and (consp value) (eq (car value) 'quote))
                             (cadr value)
                           value)))))
      (setq tail (cdr tail)))
    result))

(defun foc-test--forms-containing (head text)
  "Return each sub-form with car HEAD whose printed form contains TEXT.
Each result is the printed form, as a string.  Matching on the printed form
keeps this independent of the many shapes a `:hook' entry can take."
  (let (hits)
    (dolist (form (cfg-test-read-forms))
      (dolist (sub (cfg-test-find-all form head))
        (let ((printed (prin1-to-string sub)))
          (when (string-match-p text printed)
            (push printed hits)))))
    hits))

;;; 1 -- diff-hl global modes

(ert-deftest file-open-cost/diff-hl-globals-live-in-a-named-function ()
  "No inline lambda enables a diff-hl global mode.
The enable used to sit in a `find-file' hook lambda, so it ran for every file.
A named function can take itself off the hook; a lambda cannot."
  ;; Arrange / Act / Assert
  (should-not (foc-test--forms-containing 'lambda "global-diff-hl-mode"))
  (should-not (foc-test--forms-containing 'lambda "diff-hl-flydiff-mode")))

(ert-deftest file-open-cost/diff-hl-setup-runs-once ()
  "The diff-hl setup takes itself off `find-file-hook' before doing its work.
The hook stays the trigger, so the package still loads late rather than at
startup, but one run is enough."
  ;; Arrange / Act
  (let ((defuns (foc-test--forms-containing 'defun "global-diff-hl-mode")))
    ;; Assert
    (should (= (length defuns) 1))
    (let ((setup (car defuns)))
      (should (string-match-p "remove-hook" setup))
      (should (string-match-p "find-file-hook" setup)))))

(ert-deftest file-open-cost/flydiff-delay-is-raised ()
  "diff-hl waits at least a second before it re-diffs an edited buffer.
`diff-hl-flydiff-mode' swaps the diff source: instead of one `git diff' on the
saved file, it extracts the reference blob to a temp file with `git cat-file'
and runs the external `diff-program' against the live buffer.  At the 0.3s
default that work repeats while you type."
  ;; Arrange / Act
  (let* ((form (foc-test--use-package-form 'diff-hl))
         (value (foc-test--custom-value form 'diff-hl-flydiff-delay)))
    ;; Assert
    (should form)
    (should (numberp value))
    (should (>= value 1))))

;;; 2 -- Flycheck on open

(ert-deftest file-open-cost/flycheck-does-not-check-on-open ()
  "Opening a TS buffer does not start a check.
`mode-enabled' would start the approximately 6 second ESLint CLI run for each
TS buffer a command opens."
  ;; Arrange / Act -- the value is set with `setq-local' on the TS mode hooks.
  (let (triggers)
    (dolist (form (cfg-test-read-forms))
      (dolist (call (cfg-test-find-all form 'setq-local))
        (let ((tail (cdr call)))
          (while tail
            (when (eq (car tail) 'flycheck-check-syntax-automatically)
              (let ((value (cadr tail)))
                (setq triggers (if (and (consp value) (eq (car value) 'quote))
                                   (cadr value)
                                 value))))
            (setq tail (cddr tail))))))
    ;; Assert
    (should triggers)
    (should (memq 'save triggers))
    (should-not (memq 'mode-enabled triggers))))

(provide 'file-open-cost-test)
;;; file-open-cost-test.el ends here
