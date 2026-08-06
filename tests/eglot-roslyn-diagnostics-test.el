;;; eglot-roslyn-diagnostics-test.el --- Tests for Roslyn pull diagnostics -*- lexical-binding: t; -*-

;; Microsoft.CodeAnalysis.LanguageServer reports diagnostics by *pull* only.  A
;; probe of roslyn-ls 5.10.0 over stdio, with a project open and a file that has
;; a real compile error, shows the behaviour:
;;
;;   * it sends no `textDocument/publishDiagnostics' at all, even after
;;     `workspace/projectInitializationComplete';
;;   * it registers `textDocument/diagnostic' with `client/registerCapability'
;;     *after* the initialize response, in which `diagnosticProvider' is null;
;;   * it answers a `textDocument/diagnostic' request with kind "full" and the
;;     expected items (CS0029, CS1002, ...).
;;
;; Eglot pulls diagnostics from version 1.20, but it decides whether to pull with
;; `(eglot-server-capable :diagnosticProvider)', and `eglot-server-capable' reads
;; only the capabilities from the initialize response.  Eglot also acts on one
;; dynamic registration alone, `workspace/didChangeWatchedFiles'; every other
;; registration reaches the default method, which warns and does nothing.  So
;; Eglot never asks Roslyn, Roslyn never volunteers, and Flycheck's `eglot-check'
;; correctly reports the nothing that Eglot holds.
;;
;; Two parts fix this, and both are needed:
;;
;;   1. Eglot comes from GNU ELPA, not from the copy bundled with Emacs 30.2.
;;      The bundled 1.17.30 has no pull support at all.
;;   2. A method on `eglot-register-capability' for `textDocument/diagnostic'
;;      writes `:diagnosticProvider' into the server's capabilities, so Eglot's
;;      own pull path runs.
;;
;; The decision in part 2 is a pure function, so the merge is unit-tested without
;; a live server.  The `setf' on the server object is not covered here -- see the
;; note in the config.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun erd-test--config-loaded-p ()
  "Return non-nil if this Emacs has loaded the full configuration."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

(defun erd-test--nix-list (name)
  "Return the names in the first Nix list literal after NAME in flake.nix."
  (with-temp-buffer
    (insert-file-contents "flake.nix")
    (goto-char (point-min))
    (re-search-forward (concat (regexp-quote name) "[[:space:]]*="))
    (re-search-forward "\\[")
    (let ((start (point)))
      (re-search-forward "\\]")
      (split-string (buffer-substring-no-properties start (1- (point))) nil t))))

(defun erd-test--eglot-use-package ()
  "Return the `use-package eglot' form from config.el, or nil."
  (catch 'found
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (when (eq (nth 1 up) 'eglot)
          (throw 'found up))))
    nil))

;;; Pure -- the capability merge

(ert-deftest roslyn-diagnostics/register-options-become-the-capability ()
  "The registration's options are stored as `:diagnosticProvider'."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-capabilities-with-pull-diagnostics)
  ;; Act
  (let ((result (my/eglot-capabilities-with-pull-diagnostics
                 '(:hoverProvider t)
                 '(:interFileDependencies t :workspaceDiagnostics :json-false))))
    ;; Assert
    (should (equal (plist-get result :hoverProvider) t))
    (should (equal (plist-get result :diagnosticProvider)
                   '(:interFileDependencies t :workspaceDiagnostics :json-false)))))

(ert-deftest roslyn-diagnostics/no-options-still-enables-pull ()
  "A registration with no options gives a plain non-nil capability.
Eglot only tests the value for truth here: `eglot--flymake-pull' sends
`:textDocument' and an optional `:previousResultId', and reads none of the
registration options."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-capabilities-with-pull-diagnostics)
  ;; Act
  (let ((result (my/eglot-capabilities-with-pull-diagnostics '(:hoverProvider t) nil)))
    ;; Assert
    (should (eq (plist-get result :diagnosticProvider) t))))

(ert-deftest roslyn-diagnostics/capability-passes-the-eglot-probe ()
  "The stored value is one that `eglot-server-capable' accepts.
That function returns nil for a missing key and for `:json-false'."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-capabilities-with-pull-diagnostics)
  ;; Act
  (dolist (params (list nil '(:interFileDependencies t)))
    (let* ((result (my/eglot-capabilities-with-pull-diagnostics nil params))
           (probe (plist-member result :diagnosticProvider)))
      ;; Assert
      (should probe)
      (should-not (eq (cadr probe) :json-false))
      (should (cadr probe)))))

(ert-deftest roslyn-diagnostics/input-capabilities-are-not-mutated ()
  "The merge does not write through to the capabilities it was given.
Eglot's real plist comes from the server object, so a destructive merge would
be a surprise for callers and for this test's own fixtures."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-capabilities-with-pull-diagnostics)
  (let* ((caps (list :hoverProvider t))
         (before (copy-sequence caps)))
    ;; Act
    (my/eglot-capabilities-with-pull-diagnostics caps '(:interFileDependencies t))
    ;; Assert
    (should (equal caps before))))

;;; Structural -- the package source and the method

(ert-deftest roslyn-diagnostics/eglot-comes-from-elpa ()
  "Eglot is a Nix-managed package, not the copy bundled with Emacs.
The bundled 1.17.30 defines no pull diagnostics, so `:ensure nil' would leave
the whole feature missing."
  ;; Arrange / Act
  (let ((packages (erd-test--nix-list "dotemacsPackageList"))
        (form (erd-test--eglot-use-package)))
    ;; Assert
    (should (member "eglot" packages))
    (should form)
    ;; `:ensure nil' marks a built-in elsewhere in this config.
    (should-not (memq :ensure form))))

(ert-deftest roslyn-diagnostics/register-capability-method-is-defined ()
  "config.el defines an `eglot-register-capability' method for the pull method."
  ;; Arrange / Act
  (let ((methods (cfg-test-find-all (cons 'progn (cfg-test-read-forms))
                                    'cl-defmethod)))
    ;; Assert
    (should (cl-find-if
             (lambda (m)
               (and (eq (nth 1 m) 'eglot-register-capability)
                    (string-match-p "textDocument/diagnostic"
                                    (prin1-to-string m))))
             methods))))

(ert-deftest roslyn-diagnostics/no-obsolete-confirm-alias ()
  "The config sets `eglot-confirm-server-edits', not the obsolete alias.
`eglot-confirm-server-initiated-edits' has been an obsolete alias since Eglot
1.16."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (string-match-p "eglot-confirm-server-initiated-edits" code))))

;;; Behavioural -- the real package set

(ert-deftest roslyn-diagnostics/eglot-on-load-path-can-pull ()
  "The Eglot that loads is the ELPA one, and it has the pull machinery.
Eglot carries no version variable, so this tests for the features instead.  The
bundled 1.17.30 has neither, and it lives in the Emacs install tree rather than
in an `elpa' directory."
  ;; Arrange
  (skip-unless (erd-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Assert
  (should (fboundp 'eglot--flymake-pull))        ; Eglot 1.20
  (should (boundp 'eglot-code-action-indications)) ; Eglot 1.19
  (should (string-match-p "/elpa/" (locate-library "eglot"))))

(ert-deftest roslyn-diagnostics/method-is-registered-on-the-generic ()
  "Emacs dispatches `eglot-register-capability' to the new method."
  ;; Arrange
  (skip-unless (erd-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Assert
  (should (cl-find-method #'eglot-register-capability '()
                          (list t '(eql textDocument/diagnostic) t))))

(provide 'eglot-roslyn-diagnostics-test)
;;; eglot-roslyn-diagnostics-test.el ends here
