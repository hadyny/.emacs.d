;;; flycheck-migration-test.el --- Tests for the flymake -> flycheck 38 switch -*- lexical-binding: t; -*-

;; Specifies the move off built-in Flymake onto Flycheck 38:
;;
;; * no `flymake' / `flymake-eslint' use-package forms -- and no flymake
;;   symbols at all -- remain in the tangled config (structural, parses
;;   config.el, so it runs anywhere);
;;
;; * indicators stay in the *margin*.  `flycheck-indication-mode' defaults to
;;   `auto', which draws in the fringe -- diff-hl owns the fringe here -- and
;;   the three levels keep distinct nerd-font glyphs instead of Flycheck's
;;   single `»' (`flycheck-redefine-standard-error-levels' takes one string for
;;   all three levels, so each level needs its own `:margin-spec');
;;
;; * Eglot feeds Flycheck through v38's built-in `global-flycheck-eglot-mode',
;;   which obsoletes the third-party `flycheck-eglot' package, and
;;   `flycheck-eglot-exclusive' is nil: at its default t, `eglot-check' is the
;;   only checker in an Eglot buffer and ESLint silently stops reporting;
;;
;; * ESLint comes from Flycheck's built-in `javascript-eslint' checker on the
;;   same three tree-sitter modes `flymake-eslint' used to hook;
;;
;; * inline messages keep Flycheck's own defaults (`below' on the line at
;;   point, `eol' on every other line), so nothing pins the annotate styles.
;;
;; The behavioural half needs the real package set with Flycheck 38 on it, so
;; it keys off `my/add-node-modules-path' being on `tsx-ts-mode-hook' -- the
;; config adds it unconditionally at load, so its presence means the whole
;; config is loaded (integration-tests) and its absence means the isolated
;; emacs-nox run (unit-tests).
;;
;; The tests that inspect Flycheck's runtime state `require' it themselves:
;; the package is `:defer'red and neither `prog-mode' nor `after-init' fires in
;; batch, so nothing else loads it.  That also exercises the deferral itself --
;; the glyphs live in a `:config' block, i.e. behind `with-eval-after-load', so
;; they must land whenever Flycheck happens to load rather than at startup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                              (file-name-directory
                                               (or load-file-name buffer-file-name))))

(defun fm-test--use-package-names ()
  "Return the list of packages configured via `use-package' in config.el."
  (let (names)
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (push (nth 1 up) names)))
    names))

(defun fm-test--customs ()
  "Return an alist of (VARIABLE VALUE-FORM) from every `:custom' block in config.el.
Handles both the bare `(VAR VALUE)' entries this config uses and the
parenthesised `((VAR VALUE) ...)' shape use-package also accepts."
  (let (pairs)
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (let ((tail (cdr (memq :custom up))))
          ;; `:custom' runs until the next use-package keyword.
          (while (and tail (not (keywordp (car tail))))
            (let ((entry (car tail)))
              (cond
               ;; (VAR VALUE)
               ((and (consp entry) (car entry) (symbolp (car entry)))
                (push entry pairs))
               ;; ((VAR VALUE) (VAR VALUE) ...)
               ((consp entry)
                (dolist (inner entry) (push inner pairs)))))
            (setq tail (cdr tail))))))
    (nreverse pairs)))

(defun fm-test--custom-set-p (var)
  "Non-nil when config.el assigns VAR in a `:custom' block."
  (and (assq var (fm-test--customs)) t))

(defun fm-test--custom-value (var)
  "Return the value config.el assigns VAR in a `:custom' block, unquoted."
  (let ((value (cadr (assq var (fm-test--customs)))))
    (if (and (consp value) (eq (car value) 'quote)) (cadr value) value)))

(defun fm-test--config-code ()
  "Return config.el's forms as a string, so comments and prose cannot match."
  (prin1-to-string (cfg-test-read-forms)))

(defun fm-test--config-loaded-p ()
  "Non-nil when the full config has been loaded into this Emacs."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

;;; Structural -- the tangled config

(ert-deftest flycheck-migration/flymake-stack-removed ()
  "Nothing in the configuration references Flymake any more."
  ;; Arrange / Act
  (let ((packages (fm-test--use-package-names))
        (code (fm-test--config-code)))
    ;; Assert
    (should-not (memq 'flymake packages))
    (should-not (memq 'flymake-eslint packages))
    (should-not (string-match-p "flymake" code))))

(ert-deftest flycheck-migration/flycheck-configured ()
  "Flycheck is configured, and the superseded flycheck-eglot bridge is not."
  ;; Arrange / Act
  (let ((packages (fm-test--use-package-names)))
    ;; Assert
    (should (memq 'flycheck packages))
    ;; v38 ships the bridge; the third-party package clashes on command names.
    (should-not (memq 'flycheck-eglot packages))))

(ert-deftest flycheck-migration/indicators-stay-in-the-margin ()
  "Indicators are drawn in the left margin, leaving the fringe to diff-hl."
  ;; Arrange / Act / Assert
  (should (fm-test--custom-set-p 'flycheck-indication-mode))
  (should (eq (fm-test--custom-value 'flycheck-indication-mode) 'left-margin)))

(ert-deftest flycheck-migration/eglot-bridge-is-exclusive ()
  "The Eglot bridge is on and stays exclusive at the Flycheck default.
ESLint needed the chain before.  It then ran as `javascript-eslint' together
with Eglot.  ESLint is now a server behind the `rass' multiplexer.  A chain
would therefore report each ESLint result twice.  One result would come from
the LSP diagnostics and one from the CLI checker.  Flycheck still reaches the
checker in a buffer that Eglot does not manage, because the predicate of
`eglot-check' excludes it there.  See eglot-multiserver-test.el."
  ;; Arrange / Act
  (let ((code (fm-test--config-code)))
    ;; Assert
    (should (string-match-p "global-flycheck-eglot-mode" code))
    ;; Absent is the point -- the default is already t.
    (should-not (fm-test--custom-set-p 'flycheck-eglot-exclusive))))

(ert-deftest flycheck-migration/annotate-mode-on-at-flycheck-defaults ()
  "Inline diagnostics are enabled and left at Flycheck's own styles."
  ;; Arrange / Act
  (let ((code (fm-test--config-code)))
    ;; Assert
    (should (string-match-p "global-flycheck-annotate-mode" code))
    (should-not (fm-test--custom-set-p 'flycheck-annotate-current-line-style))
    (should-not (fm-test--custom-set-p 'flycheck-annotate-other-lines-style))))

;;; Behavioural -- the real package set, fully loaded config

(ert-deftest flycheck-migration/v38-features-available ()
  "The Flycheck on the load-path is v38: it has the bridge, annotate and fixes.
Guards the flake's `flycheck' pin -- the version nixpkgs packages predates
38.0 and defines none of these."
  ;; Arrange
  (skip-unless (fm-test--config-loaded-p))
  ;; Assert
  (should (fboundp 'global-flycheck-eglot-mode))
  (should (fboundp 'global-flycheck-annotate-mode))
  (should (fboundp 'flycheck-fix-error-at-point)))

(ert-deftest flycheck-migration/enabled-on-prog-mode ()
  "Flycheck replaces Flymake as the `prog-mode' diagnostics backend."
  ;; Arrange
  (skip-unless (fm-test--config-loaded-p))
  ;; Assert
  (should (memq 'flycheck-mode prog-mode-hook))
  (should-not (memq 'flymake-mode prog-mode-hook)))

(ert-deftest flycheck-migration/eslint-checker-covers-the-ts-modes ()
  "The built-in ESLint checker covers the modes flymake-eslint used to hook."
  ;; Arrange
  (skip-unless (fm-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Assert
  (should (memq 'javascript-eslint flycheck-checkers))
  (dolist (mode '(tsx-ts-mode typescript-ts-mode js-ts-mode))
    (should (flycheck-checker-supports-major-mode-p 'javascript-eslint mode)))
  ;; In a buffer that Eglot manages, ESLint comes from the multiplexer.  A chain
  ;; to the CLI checker would therefore report each result twice.
  (should (eq flycheck-eglot-exclusive t)))

(ert-deftest flycheck-migration/error-levels-keep-distinct-glyphs ()
  "error/warning/info each carry their own margin glyph, not Flycheck's `»'."
  ;; Arrange
  (skip-unless (fm-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Act
  (let ((glyphs (mapcar (lambda (level)
                          (substring-no-properties
                           (or (flycheck-error-level-margin-spec level) "")))
                        '(error warning info))))
    ;; Assert
    (dolist (glyph glyphs)
      (should-not (equal glyph ""))
      (should-not (equal glyph flycheck-default-margin-str)))
    (should (equal glyphs (cl-remove-duplicates glyphs :test #'equal)))))

(provide 'flycheck-migration-test)
;;; flycheck-migration-test.el ends here
