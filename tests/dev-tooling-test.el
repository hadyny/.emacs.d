;;; dev-tooling-test.el --- Tests for the development tooling -*- lexical-binding: t; -*-

;; Six additions, each closing a gap that cost something concrete.
;;
;; * yasnippet.  Eglot advertises `snippetSupport' from whether a snippet expander
;;   exists -- `eglot--snippet-expansion-fn' is `(and (fboundp 'yas-minor-mode)
;;   ...)'.  Without it the servers are told this client cannot expand snippets,
;;   so a completed function arrives as a bare identifier with no parameter
;;   tabstops.  An autoload satisfies `fboundp', so the package may stay deferred.
;;
;; * compile / recompile bindings, so a build or test suite can be run from inside
;;   Emacs and `next-error' has a compilation buffer to walk.
;;
;; * evil-surround, wgrep, jinx, editorconfig.  See the config for each.  `wgrep'
;;   is what makes a saved Helm grep buffer editable, which is the
;;   project-wide refactor path this config was one package short of.
;;
;; `envrc' is deliberately absent, and asserted so: it shells out to direnv
;; synchronously, so Emacs blocks for the whole run -- seconds on a warm nix
;; evaluation, a full build on a project whose devShell is not yet built.
;;
;; Structural unless noted: these read the tangled config.el and flake.nix.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defconst dt-test--packages
  '("yasnippet" "yasnippet-snippets" "evil-surround" "wgrep" "jinx")
  "Packages the additions need from the Nix set.  editorconfig is built in.")

(defun dt-test--config-loaded-p ()
  "Return non-nil if this Emacs has loaded the full configuration."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

(defun dt-test--use-package-names ()
  "Return the packages configured with `use-package' in config.el."
  (let (names)
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (push (nth 1 up) names)))
    names))

(defun dt-test--bindings ()
  "Return the printed form of every `evil-define-key' call in config.el."
  (mapcar #'prin1-to-string
          (cfg-test-find-all (cons 'progn (cfg-test-read-forms)) 'evil-define-key)))

;;; Packages are in the closure and configured

(ert-deftest dev-tooling/packages-are-in-the-closure ()
  "Every added package is in `dotemacsPackageList'."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList")))
    ;; Assert
    (dolist (p dt-test--packages)
      (should (member p packages)))))

(ert-deftest dev-tooling/packages-are-configured ()
  "Every added package has a `use-package' form, and editorconfig is built in."
  ;; Arrange / Act
  (let ((configured (dt-test--use-package-names)))
    ;; Assert
    (dolist (p dt-test--packages)
      (should (memq (intern p) configured)))
    (should (memq 'editorconfig configured))))

;;; Snippets

(ert-deftest dev-tooling/snippets-satisfy-eglots-probe ()
  "yasnippet is wired so Eglot can advertise `snippetSupport'.
Eglot tests `(fboundp \\='yas-minor-mode)', which an autoload satisfies, so this
holds even though the package is deferred."
  ;; Arrange
  (skip-unless (dt-test--config-loaded-p))
  ;; Assert
  (should (fboundp 'yas-minor-mode))
  (should (require 'eglot nil t))
  (should (eglot--snippet-expansion-fn)))

;;; Build and test

(ert-deftest dev-tooling/compile-and-recompile-are-bound ()
  "There is a way to run a build or a test suite, and to repeat it."
  ;; Arrange / Act
  (let ((code (mapconcat #'identity (dt-test--bindings) " ")))
    ;; Assert
    (should (string-match-p "\\bcompile\\b" code))
    (should (string-match-p "recompile" code))))

;;; direnv

(ert-deftest dev-tooling/direnv-integration-is-not-wired ()
  "envrc stays out: it blocks Emacs for a whole synchronous direnv run.
Both halves must go, or the closure keeps a `direnv' nothing uses."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
        (tools (cfg-test-nix-list "emacsToolsFor"))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (member "envrc" packages))
    (should-not (member "direnv" tools))
    (should-not (string-match-p "envrc" code))))

;;; Spelling

(ert-deftest dev-tooling/spelling-covers-prose-not-code ()
  "jinx runs in prose modes, and is not turned on globally.
Spell-checking every code buffer is noise; the value is in Org, Markdown, notes
and commit messages."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should (string-match-p "jinx-mode" code))
    (should-not (string-match-p "global-jinx-mode" code))))

(ert-deftest dev-tooling/spelling-language-has-a-backend ()
  "`jinx-languages' is set away from the locale default, which has no backend.
The locale here is en_NZ and no enchant provider offers it; jinx warns once and
then silently checks nothing.  en_GB is available from AppleSpell on macOS and
accepts NZ spelling."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should (string-match-p "jinx-languages \"en_GB\"" code))))

(ert-deftest dev-tooling/spelling-actually-resolves-a-dictionary ()
  "The configured language resolves to a real dictionary that catches errors.
Guards the whole chain: the dynamic module loads, enchant finds a provider, and
the provider accepts NZ spelling rather than flagging it."
  ;; Arrange
  (skip-unless (dt-test--config-loaded-p))
  (should (require 'jinx nil t))
  (with-temp-buffer
    (text-mode)
    ;; `jinx-mode' is what loads the dynamic module.
    (jinx-mode 1)
    (skip-unless (fboundp 'jinx--mod-dict))
    ;; Act
    (let ((dict (jinx--mod-dict jinx-languages)))
      ;; Assert -- two ways a host can have no usable backend, both skipped
      ;; rather than failed so this still asserts something real where one
      ;; exists:
      ;;   * no dictionary at all.  macOS gets en_GB from AppleSpell for free;
      ;;     elsewhere enchant needs a hunspell dictionary on its search path,
      ;;     which the home-manager module installs on Linux and the
      ;;     integration-tests check provides for CI.
      ;;   * a dictionary that resolves but answers nothing.  AppleSpell needs
      ;;     the macOS spell service, which the Nix build sandbox cannot reach,
      ;;     so there it accepts every word including a misspelt one.
      (skip-unless dict)
      (skip-unless (not (jinx--mod-check dict "mispelled")))
      (should (jinx--mod-check dict "sentence"))
      ;; NZ spelling must not be flagged -- see the repo's own conventions.
      (should (jinx--mod-check dict "organisation"))
      (should (jinx--mod-check dict "colour")))))

;;; Behavioural -- the real package set

(ert-deftest dev-tooling/entry-points-are-autoloadable ()
  "Each addition's entry point is reachable without an explicit `require'.
Mirrors the `packages-loadable' flake check: a `:config'/`:init' call on a
package whose autoloads are missing is a void-function at startup."
  ;; Arrange
  (skip-unless (dt-test--config-loaded-p))
  ;; Assert
  (dolist (fn '(yas-minor-mode
                yas-global-mode
                global-evil-surround-mode
                wgrep-setup
                jinx-mode
                editorconfig-mode))
    (should (fboundp fn))))

(ert-deftest dev-tooling/shell-path-is-imported-in-a-daemon ()
  "`exec-path-from-shell' runs in a daemon, not only in a windowed Emacs.
A launchd agent inherits a minimal PATH, so the shell PATH has to be imported or
nothing on it is found -- every language server included.  Guarding only on
`window-system' skips that: a daemon has no frame until a client connects, so
`window-system' and `initial-window-system' are both nil while `(daemonp)' is t.
Verified against a real `--daemon':

  ((daemonp . t) (window-system) (initial-window-system))"
  ;; Arrange / Act
  (let ((form (catch 'found
                (dolist (f (cfg-test-read-forms))
                  (dolist (up (cfg-test-find-all f 'use-package))
                    (when (eq (nth 1 up) 'exec-path-from-shell)
                      (throw 'found up))))
                nil)))
    ;; Assert
    (should form)
    (let ((printed (prin1-to-string form)))
      (should (string-match-p "exec-path-from-shell-initialize" printed))
      (should (string-match-p "daemonp" printed)))))

(ert-deftest dev-tooling/nix-formatting-matches-the-repo-formatter ()
  "`nixfmt' is on PATH so Apheleia can format .nix on save.
Apheleia already ships the formatter and the mode mapping -- `(nixfmt
. (\"nixfmt\"))' plus `nix-mode' and `nix-ts-mode' -- so only the binary is
needed and no `apheleia-formatters' entry belongs here.  It is the same nixfmt
that `nix fmt' (nixfmt-tree) runs, so format-on-save and the pre-commit hook
cannot disagree."
  ;; Arrange / Act
  (let ((tools (cfg-test-nix-list "emacsToolsFor"))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should (member "nixfmt" tools))
    ;; Apheleia knows nix already; a local override would be dead weight.
    (should-not (string-match-p "apheleia-formatters" code))
    (should-not (string-match-p "apheleia-mode-alist" code))))

(provide 'dev-tooling-test)
;;; dev-tooling-test.el ends here
