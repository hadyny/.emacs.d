;;; org-capture-test.el --- Tests for org-capture-templates -*- lexical-binding: t; -*-

;; The "Work Log Entry" template used `file+datetree', a deprecated alias
;; `org-capture-upgrade-templates' silently rewrites to `file+olp+datetree' at
;; capture time.  That rewrite happens too late for `setopt', which validates
;; the raw value against `org-capture-templates's `:type' the moment it is
;; set -- and that `:type' only lists `file+olp+datetree', not the alias -- so
;; Emacs warned on every startup:
;;
;;   Warning (emacs): Value ... for `org-capture-templates' does not match type ...
;;
;; Using `file+olp+datetree' (with no outline path, i.e. a top-level date
;; tree) directly is a no-op behaviour change and silences the warning.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest org-capture/no-deprecated-datetree-alias ()
  "The config uses `file+olp+datetree', not the deprecated `file+datetree'.
`file+olp+datetree' does not contain `file+datetree' as a substring (the
`olp+' sits in between), so this is a genuine check for the alias, not a
false match against its replacement."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (string-match-p (regexp-quote "file+datetree") code))))

(ert-deftest org-capture/work-log-uses-olp-datetree ()
  "The Work Log Entry template's target is the current `file+olp+datetree'."
  ;; Arrange
  (let ((forms (cfg-test-read-forms))
        found)
    ;; Act
    (dolist (setopt (cfg-test-find-all forms 'setopt))
      (when (memq 'org-capture-templates setopt)
        (setq found (cadr (memq 'org-capture-templates setopt)))))
    ;; Assert
    (should found)
    (let ((work-log (assoc "w" (cadr found))))
      (should work-log)
      (should (equal (nth 3 work-log) '(file+olp+datetree "~/notes/org/work-log.org"))))))

(provide 'org-capture-test)
;;; org-capture-test.el ends here
