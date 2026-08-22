;;; agent-shell-backends-test.el --- Which agent-shell backends are wired -*- lexical-binding: t; -*-

;; agent-shell ships many ACP backends; only the ones whose agent binary is
;; actually in the tool closure are worth exposing, so every backend reference
;; has to stay in step with `emacsToolsFor' in flake.nix.  Copilot and Claude
;; Code are wired.
;;
;; The Google backend is deliberately not: Antigravity is what Google is
;; migrating Gemini CLI's users to, agent-shell has no dedicated Antigravity
;; backend yet, and Antigravity CLI (`agy') has no ACP support to wire against
;; -- confirmed directly: neither `agy --help' nor a `strings' scan of the
;; binary shows an ACP flag or library reference (Antigravity CLI 1.1.13), and
;; `agy --acp' itself just errors with "flags provided but not defined".  A
;; bound-but-broken start command is worse than an absent one, and a binary
;; nobody invokes is the same drift in the other direction.  Both are asserted
;; absent, on both sides.

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

(ert-deftest agent-shell-backends/claude-code-is-wired ()
  "The Claude Code start command is exposed, and its adapter is in the closure."
  ;; Arrange / Act
  (let ((commands (as-test--commands))
        (tools (cfg-test-nix-list "emacsToolsFor")))
    ;; Assert
    (should (memq 'agent-shell-anthropic-start-claude-code commands))
    (should (member "claude-agent-acp" tools))))

(ert-deftest agent-shell-backends/claude-code-prefers-path-executable ()
  "`claude-agent-acp' is pointed at whatever `claude' resolves on PATH.
Otherwise it silently falls back to nixpkgs' own `claude-code' build --
a second, independently-updated copy of the same tool -- rather than the one
the home-manager profile actually manages."
  ;; Arrange / Act
  (let* ((form (as-test--agent-shell-form))
         (printed (prin1-to-string form)))
    ;; Assert
    (should form)
    (should (string-match-p "agent-shell-anthropic-claude-environment" printed))
    (should (string-match-p "CLAUDE_CODE_EXECUTABLE" printed))
    (should (string-match-p "executable-find \"claude\"" printed))))

(ert-deftest agent-shell-backends/gemini-backend-not-wired ()
  "Nothing references the Gemini backend."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (string-match-p "agent-shell-google-" code))))

(ert-deftest agent-shell-backends/tool-closure-matches-the-wired-backends ()
  "The closure carries the Copilot and Claude Code binaries and no Gemini or
Antigravity one."
  ;; Arrange / Act
  (let ((tools (cfg-test-nix-list "emacsToolsFor")))
    ;; Assert
    (should (member "github-copilot-cli" tools))
    (should (member "claude-agent-acp" tools))
    (should-not (member "gemini-cli" tools))
    (should-not (member "antigravity-cli" tools))))

(provide 'agent-shell-backends-test)
;;; agent-shell-backends-test.el ends here
