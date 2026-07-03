;;; evil-undo-system-test.el --- Tests for the configured Evil undo system -*- lexical-binding: t; -*-

;; `evil-set-undo-system' does not validate its argument: it merely points
;; `evil-undo-function'/`evil-redo-function' at the undo/redo commands for the
;; named system. If that system's package is absent (as `undo-tree' once was
;; here), those functions are void and *every* undo/redo keystroke errors --
;; silently at startup, loudly on first use. This test reads the system the
;; config selects and asserts it resolves to defined functions.
;;
;; Needs Evil loaded, so it self-skips on a bare `emacs-nox' run.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun eus-test--configured-undo-system ()
  "Return the symbol X from (evil-set-undo-system 'X) in config.el, or :none."
  (let ((result :none))
    (dolist (form (cfg-test-read-forms))
      (dolist (call (cfg-test-find-all form 'evil-set-undo-system))
        (let ((arg (nth 1 call)))
          (setq result (if (and (consp arg) (eq (car arg) 'quote))
                           (cadr arg)
                         arg)))))
    result))

(ert-deftest evil-undo/functions-are-defined ()
  "The undo system the config selects resolves to defined undo/redo functions."
  ;; Arrange: evil-set-undo-system is not autoloaded, so pull in Evil first;
  ;; on a bare emacs-nox run this require fails and the test self-skips.
  (require 'evil nil t)
  (skip-unless (fboundp 'evil-set-undo-system))
  (let ((system (eus-test--configured-undo-system)))
    (skip-unless (not (eq system :none)))
    ;; Act
    (evil-set-undo-system system)
    ;; Assert
    (should (fboundp evil-undo-function))
    (should (fboundp evil-redo-function))))

(provide 'evil-undo-system-test)
;;; evil-undo-system-test.el ends here
