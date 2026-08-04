;;; eglot-migration-test.el --- Tests for the lsp-mode -> eglot migration -*- lexical-binding: t; -*-

;; Specifies the move off lsp-mode onto eglot + Flycheck's ESLint checker:
;;
;; * no lsp-mode / lsp-tailwindcss / lsp-eslint use-package forms remain
;;   (structural, parses the tangled config.el -- runs anywhere);
;; * eglot is NOT auto-started -- it is launched manually via M-x eglot, so the
;;   TypeScript/TSX hooks must not run eglot-ensure (behavioural -- needs the
;;   full config loaded, so it self-skips on a bare emacs-nox run);
;; * ESLint is not a Flymake backend.  See flycheck-migration-test.el.
;;
;; ESLint has moved twice since this file was written.  This file therefore no
;; longer specifies where ESLint runs.  It specifies only that Flymake is not
;; involved.  ESLint was `lsp-eslint' with lsp-mode.  It then became the
;; Flycheck `javascript-eslint' checker.  It is now a server behind the `rass'
;; multiplexer, with typescript-language-server and tailwindcss-language-server.
;; The CLI checker runs `eslint --format=json --stdin' on each idle-change and
;; new-line.  A type-aware typescript-eslint configuration makes each run take
;; approximately 6 seconds.  The checker stays registered as the fallback when
;; no connection is up.  See eglot-multiserver-test.el.
;;
;; The behavioural tests key off `my/add-node-modules-path' being on
;; `tsx-ts-mode-hook' -- the config adds it unconditionally at load, so its
;; presence means the whole config has been loaded (integration-tests), and its
;; absence means we are in the isolated emacs-nox run (unit-tests).

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun em-test--use-package-names ()
  "Return the list of packages configured via `use-package' in config.el."
  (let (names)
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (push (nth 1 up) names)))
    names))

(defun em-test--config-loaded-p ()
  "Non-nil when the full config has been loaded into this Emacs."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

(ert-deftest eglot-migration/no-lsp-packages ()
  "The lsp-mode stack is fully removed from the configuration."
  ;; Arrange / Act
  (let ((packages (em-test--use-package-names)))
    ;; Assert
    (should-not (memq 'lsp-mode packages))
    (should-not (memq 'lsp-tailwindcss packages))
    (should-not (memq 'lsp-eslint packages))))

(ert-deftest eglot-migration/eglot-started-manually ()
  "eglot is NOT auto-started; the TypeScript/TSX hooks don't run eglot-ensure."
  ;; Arrange
  (skip-unless (em-test--config-loaded-p))
  ;; Assert
  (should-not (memq 'eglot-ensure tsx-ts-mode-hook))
  (should-not (memq 'eglot-ensure typescript-ts-mode-hook)))

(ert-deftest eglot-migration/eslint-not-a-flymake-backend ()
  "ESLint reaches Flycheck and never Flymake, whatever supplies it.
For the place where ESLint runs, see eglot-multiserver-test.el.  This test
specifies only that `flymake-eslint' is absent from the TS hooks."
  ;; Arrange -- the flycheck block is `:defer'red and no hook that would load it
  ;; fires in batch, so load it here.
  (skip-unless (em-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Assert
  (should-not (memq 'flymake-eslint-enable tsx-ts-mode-hook))
  (should-not (memq 'flymake-eslint-enable typescript-ts-mode-hook))
  (should-not (memq 'flymake-mode tsx-ts-mode-hook)))

(provide 'eglot-migration-test)
;;; eglot-migration-test.el ends here
