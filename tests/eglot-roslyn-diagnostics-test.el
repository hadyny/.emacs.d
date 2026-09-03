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

;; A third part is needed before any of that reaches the screen.  Flycheck 38.3's
;; bridge drops Eglot's `:region' argument:
;;
;;   (defun flycheck-eglot--report (diags &rest _)
;;     (setq flycheck-eglot--diagnostics (append diags nil))
;;     ...)
;;
;; Eglot reports nil with a degenerate `:region' to mean "keep what you have".
;; It does that whenever a pull answers "unchanged", which Roslyn does for the
;; second pull of an unedited buffer -- and Flycheck itself makes that second
;; pull, because the first report triggers a re-check.  So the diagnostics
;; arrive and are wiped about a second later:
;;
;;   PULL id=99  kind=full      items=3
;;   PULL id=100 kind=unchanged items=0    <- reported as nil, cache cleared
;;
;; The advice below restores the meaning of `:stay'.  This is a Flycheck bug and
;; affects every pull server, not only Roslyn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun erd-test--config-loaded-p ()
  "Return non-nil if this Emacs has loaded the full configuration."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

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

;;; Pure -- the `:stay' fix for Flycheck's bridge

(defvar flycheck-eglot--diagnostics nil)   ; declared by Flycheck when it loads

(defun erd-test--call-report (cached diags &rest args)
  "Call the advice with CACHED diagnostics, DIAGS and ARGS.
Return (CALLED-P . ARGS-PASSED-TO-ORIG)."
  (cfg-test-load-defun 'my/flycheck-eglot-report-honour-stay)
  (with-temp-buffer
    (setq-local flycheck-eglot--diagnostics cached)
    (let (called passed)
      (apply #'my/flycheck-eglot-report-honour-stay
             (lambda (d &rest a) (setq called t passed (cons d a)))
             diags args)
      (cons called passed))))

(ert-deftest roslyn-diagnostics/stay-with-no-diagnostics-keeps-the-cache ()
  "An \"unchanged\" pull must not clear the diagnostics already shown.
Eglot marks it with a degenerate region: `:region (POS . POS)'."
  ;; Arrange / Act
  (let ((result (erd-test--call-report '(a b c) nil :region '(1 . 1))))
    ;; Assert -- the original never runs, so the cache and the display stand.
    (should-not (car result))))

(ert-deftest roslyn-diagnostics/stay-with-diagnostics-adds-to-the-cache ()
  "A `:stay' report carrying diagnostics adds to them, and does not replace.
Eglot uses `:stay' to fold pushed diagnostics in beside pulled ones."
  ;; Arrange / Act
  (let* ((result (erd-test--call-report '(a b) '(c) :region '(1 . 1)))
         (passed (cdr result)))
    ;; Assert
    (should (car result))
    (should (equal (car passed) '(a b c)))))

(ert-deftest roslyn-diagnostics/clear-replaces-the-cache ()
  "A `:clear' report is the normal full update and passes straight through.
Eglot marks it with the whole buffer: `:region (POINT-MIN . POINT-MAX)'."
  ;; Arrange / Act
  (let* ((result (erd-test--call-report '(a b) '(x y) :region '(1 . 99)))
         (passed (cdr result)))
    ;; Assert
    (should (car result))
    (should (equal (car passed) '(x y)))))

(ert-deftest roslyn-diagnostics/report-without-a-region-passes-through ()
  "Eglot omits `:region' entirely for a plain report, which is a full update."
  ;; Arrange / Act
  (let* ((result (erd-test--call-report '(a b) '(x)))
         (passed (cdr result)))
    ;; Assert
    (should (car result))
    (should (equal (car passed) '(x)))))

;;; Pure -- re-checking once the workspace has loaded

(ert-deftest roslyn-diagnostics/recheck-covers-buffers-of-this-server ()
  "A managed buffer of SERVER with Flycheck on is re-checked."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-recheck-managed-buffers)
  (let ((server 'srv) rechecked)
    (with-temp-buffer
      (rename-buffer "recheck-hit.cs" t)
      (setq-local flycheck-mode t)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-current-server) (lambda () server))
                ((symbol-function 'flycheck-buffer-automatically)
                 (lambda (&rest _) (push (buffer-name) rechecked))))
        ;; Act
        (my/eglot-recheck-managed-buffers server))
      ;; Assert
      (should (member (buffer-name) rechecked)))))

(ert-deftest roslyn-diagnostics/recheck-skips-other-servers-and-plain-buffers ()
  "Buffers of another server, and buffers without Flycheck, are left alone.
`eglot-current-server' resolves by project, so it answers for buffers Eglot
does not manage -- hence the `eglot-managed-p' guard as well."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-recheck-managed-buffers)
  (let (rechecked)
    (with-temp-buffer
      (rename-buffer "recheck-miss.cs" t)
      (setq-local flycheck-mode t)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-current-server) (lambda () 'other-server))
                ((symbol-function 'flycheck-buffer-automatically)
                 (lambda (&rest _) (push (buffer-name) rechecked))))
        ;; Act
        (my/eglot-recheck-managed-buffers 'srv))
      ;; Assert -- wrong server
      (should-not (member (buffer-name) rechecked)))
    (with-temp-buffer
      (rename-buffer "recheck-noflycheck.cs" t)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-current-server) (lambda () 'srv))
                ((symbol-function 'flycheck-buffer-automatically)
                 (lambda (&rest _) (push (buffer-name) rechecked))))
        ;; Act
        (my/eglot-recheck-managed-buffers 'srv))
      ;; Assert -- Flycheck is not on in this buffer
      (should-not (member (buffer-name) rechecked)))))

;;; Structural -- the package source and the method

(ert-deftest roslyn-diagnostics/eglot-comes-from-elpa ()
  "Eglot is a Nix-managed package, not the copy bundled with Emacs.
The bundled 1.17.30 defines no pull diagnostics, so `:ensure nil' would leave
the whole feature missing."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
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

(ert-deftest roslyn-diagnostics/confirm-edits-is-a-valid-choice ()
  "`eglot-confirm-server-edits' is set to a real member of its `:type' choice.
`t' used to work here (the docstring's catch-all still honours it), but is not
among the choices the `defcustom' lists, so `setopt' warned on startup; `summary'
is the literal choice with the same meaning."
  ;; Arrange
  (let ((forms (cfg-test-read-forms))
        found)
    ;; Act
    (dolist (setopt (cfg-test-find-all forms 'setopt))
      (when (memq 'eglot-confirm-server-edits setopt)
        (setq found (cadr (memq 'eglot-confirm-server-edits setopt)))))
    ;; Assert
    (should (equal found (list 'quote 'summary)))))

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

(ert-deftest roslyn-diagnostics/init-complete-notification-is-handled ()
  "config.el defines a handler for Roslyn's project-initialisation notification.
Nothing else re-checks when the workspace finishes loading: Roslyn does not
push, and the first pull races the load and comes back empty."
  ;; Arrange / Act
  (let ((methods (cfg-test-find-all (cons 'progn (cfg-test-read-forms))
                                    'cl-defmethod)))
    ;; Assert
    (should (cl-find-if
             (lambda (m)
               (and (eq (nth 1 m) 'eglot-handle-notification)
                    (string-match-p "workspace/projectInitializationComplete"
                                    (prin1-to-string m))))
             methods))))

(ert-deftest roslyn-diagnostics/init-complete-method-is-registered ()
  "Emacs dispatches the notification to the new method."
  ;; Arrange
  (skip-unless (erd-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Assert
  (should (cl-find-method
           #'eglot-handle-notification '()
           (list t '(eql workspace/projectInitializationComplete)))))

(ert-deftest roslyn-diagnostics/stay-advice-is-installed ()
  "The `:stay' advice is attached to Flycheck's bridge once Flycheck loads."
  ;; Arrange
  (skip-unless (erd-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Assert
  (should (advice-member-p #'my/flycheck-eglot-report-honour-stay
                           'flycheck-eglot--report)))

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
