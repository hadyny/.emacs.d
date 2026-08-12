;;; agent-shell-backends-test.el --- Which agent-shell backends are wired -*- lexical-binding: t; -*-

;; agent-shell ships many ACP backends; only the ones whose agent binary is
;; actually in the tool closure are worth exposing, so every backend reference
;; has to stay in step with `emacsToolsFor' in flake.nix.  Copilot is wired.
;;
;; The Anthropic and Gemini backends are deliberately not: a bound-but-broken
;; start command is worse than an absent one, and a binary nobody invokes is the
;; same drift in the other direction.  Both are asserted absent, on both sides.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                              (file-name-directory
                                               (or load-file-name buffer-file-name))))

(defun as-test--agent-shell-form ()
  "Return the `use-package agent-shell' form from config.el, or nil."
  (catch 'found
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (when (eq (nth 1 up) 'agent-shell)
          (throw 'found up))))
    nil))

(defun as-test--commands ()
  "Return the `:commands' list of the `use-package agent-shell' form."
  (let* ((form (as-test--agent-shell-form))
         (tail (cdr (memq :commands form))))
    (car tail)))

(ert-deftest agent-shell-backends/copilot-is-wired ()
  "The Copilot start command is exposed, alongside `agent-shell'."
  ;; Arrange / Act
  (let ((commands (as-test--commands)))
    ;; Assert
    (should (memq 'agent-shell commands))
    (should (memq 'agent-shell-github-start-copilot commands))))

(ert-deftest agent-shell-backends/gemini-backend-not-wired ()
  "Nothing references the Gemini backend."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (string-match-p "agent-shell-google-" code))))

(ert-deftest agent-shell-backends/tool-closure-matches-the-wired-backends ()
  "The closure carries the Copilot binary and no longer carries the Gemini one."
  ;; Arrange / Act
  (let ((tools (cfg-test-nix-list "emacsToolsFor")))
    ;; Assert
    (should (member "github-copilot-cli" tools))
    (should-not (member "gemini-cli" tools))))

(ert-deftest agent-shell-backends/claude-backend-not-wired ()
  "Nothing references the Anthropic backend: its binary is not in the closure."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (string-match-p "agent-shell-anthropic-" code))
    (should-not (string-match-p "claude-agent-acp" code))))

(provide 'agent-shell-backends-test)
;;; agent-shell-backends-test.el ends here
