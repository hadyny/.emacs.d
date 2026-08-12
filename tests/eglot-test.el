;;; eglot-test.el --- Tests for the Eglot setup -*- lexical-binding: t; -*-

;; Eglot is not auto-started: it is launched by hand with `M-x eglot', so the
;; TypeScript/TSX hooks must not run `eglot-ensure'.  The rest of the Eglot
;; specification lives in eglot-multiserver-test.el (the TS/TSX servers),
;; eglot-roslyn-diagnostics-test.el (C# pull diagnostics) and
;; eglot-watch-glob-test.el.
;;
;; The absence tests keep the lsp-mode stack and Flymake out.  Both were replaced
;; deliberately; a stray `use-package' form or a `flymake-eslint' hook would
;; quietly run a second diagnostics system alongside the current one.
;;
;; The behavioural tests key off `my/add-node-modules-path' being on
;; `tsx-ts-mode-hook' -- the config adds it unconditionally at load, so its
;; presence means the whole config is loaded (integration-tests) and its absence
;; means the isolated emacs-nox run (unit-tests).

;;; Code:

(require 'ert)
(require 'cl-lib)
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

(ert-deftest eglot/no-lsp-packages ()
  "The lsp-mode stack stays out; Eglot replaced it."
  ;; Arrange / Act
  (let ((packages (em-test--use-package-names)))
    ;; Assert
    (should-not (memq 'lsp-mode packages))
    (should-not (memq 'lsp-tailwindcss packages))
    (should-not (memq 'lsp-eslint packages))))

(ert-deftest eglot/eglot-started-manually ()
  "eglot is NOT auto-started; the TypeScript/TSX hooks don't run eglot-ensure."
  ;; Arrange
  (skip-unless (em-test--config-loaded-p))
  ;; Assert
  (should-not (memq 'eglot-ensure tsx-ts-mode-hook))
  (should-not (memq 'eglot-ensure typescript-ts-mode-hook)))

(ert-deftest eglot/eslint-not-a-flymake-backend ()
  "Diagnostics reach Flycheck, never Flymake."
  ;; Arrange -- the flycheck block is `:defer'red and no hook that would load it
  ;; fires in batch, so load it here.
  (skip-unless (em-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Assert
  (should-not (memq 'flymake-eslint-enable tsx-ts-mode-hook))
  (should-not (memq 'flymake-eslint-enable typescript-ts-mode-hook))
  (should-not (memq 'flymake-mode tsx-ts-mode-hook)))

(ert-deftest eglot/nix-server-is-in-the-closure ()
  "A Nix language server is on PATH, and Eglot needs no entry for it.
Eglot 1.24 already maps `nix-mode' to an `eglot-alternatives' list of
\\=`nil', \\=`rnix-lsp' and \\=`nixd', and picks the first of those it can find --
so the closure supplies one and `eglot-server-programs' stays untouched.
\\=`nil' is first in that order and needs no configuration, unlike \\=`nixd',
which evaluates the flake."
  ;; Arrange / Act
  (let ((tools (cfg-test-nix-list "emacsToolsFor"))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should (cl-intersection '("nil" "nixd" "rnix-lsp") tools :test #'equal))
    ;; No hand-written entry: Eglot's own alternatives cover this.
    (should-not (string-match-p "nix-mode[^)]*rnix\\|nixd\\|\"nil\"" code))))

(provide 'eglot-test)
;;; eglot-test.el ends here
