;;; agent-shell-backends-test.el --- Which agent-shell backends are wired -*- lexical-binding: t; -*-

;; Structural spec for the agent-shell block in config.org.  agent-shell ships
;; seventeen ACP backends; only the ones whose agent binary is actually reachable
;; are worth exposing, and every reference to a backend has to stay in step with
;; the tool closure in flake.nix.
;;
;; * Copilot (`agent-shell-github-start-copilot', which runs `copilot --acp')
;;   and Gemini are wired;
;; * the Anthropic/Claude backend is deliberately NOT wired -- its
;;   `claude-agent-acp' binary is out of the tool closure, and a bound-but-broken
;;   start command is worse than an absent one.  This asserts the whole
;;   `agent-shell-anthropic-' prefix is gone, so the prose defcustom mentions
;;   cannot quietly come back either.
;;
;; Parses the tangled config.el, so it runs anywhere -- no package set needed.

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

(ert-deftest agent-shell-backends/copilot-and-gemini-are-wired ()
  "The Copilot and Gemini start commands are exposed, alongside `agent-shell'."
  ;; Arrange / Act
  (let ((commands (as-test--commands)))
    ;; Assert
    (should (memq 'agent-shell commands))
    (should (memq 'agent-shell-github-start-copilot commands))
    (should (memq 'agent-shell-google-start-gemini commands))))

(ert-deftest agent-shell-backends/claude-backend-not-wired ()
  "Nothing references the Anthropic backend: its binary is not in the closure."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (string-match-p "agent-shell-anthropic-" code))
    (should-not (string-match-p "claude-agent-acp" code))))

(provide 'agent-shell-backends-test)
;;; agent-shell-backends-test.el ends here
