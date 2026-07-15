;;; ghostel-test.el --- Tests for the ghostel terminal setup -*- lexical-binding: t; -*-

;; Specifies the ghostel install:
;;
;; * ghostel and its evil integration (evil-ghostel) are configured via
;;   use-package (structural, parses tangled config.el -- runs anywhere);
;; * evil-ghostel-mode is wired to ghostel-mode so evil support turns on in
;;   terminal buffers (behavioural -- needs the full config loaded, so it
;;   self-skips on a bare emacs-nox run).

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun ghostel-test--use-package-names ()
  "Return the list of packages configured via `use-package' in config.el."
  (let (names)
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (push (nth 1 up) names)))
    names))

(defun ghostel-test--config-loaded-p ()
  "Non-nil when the full config has been loaded into this Emacs."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

(ert-deftest ghostel/configured ()
  "ghostel and evil-ghostel are configured via use-package."
  ;; Arrange / Act
  (let ((packages (ghostel-test--use-package-names)))
    ;; Assert
    (should (memq 'ghostel packages))
    (should (memq 'evil-ghostel packages))))

(ert-deftest ghostel/evil-integration-wired ()
  "evil-ghostel-mode is enabled from ghostel-mode so evil works in terminals."
  ;; Arrange: evil-ghostel is `:after (ghostel evil)', so its hook wiring only
  ;; fires once both are loaded (as when you actually launch M-x ghostel with
  ;; evil active). Load them to trigger the deferred `with-eval-after-load'.
  (skip-unless (ghostel-test--config-loaded-p))
  (skip-unless (and (require 'evil nil t) (require 'ghostel nil t)))
  ;; Assert
  (should (memq 'evil-ghostel-mode ghostel-mode-hook)))

(provide 'ghostel-test)
;;; ghostel-test.el ends here
