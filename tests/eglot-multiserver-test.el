;;; eglot-multiserver-test.el --- Tests for the TS/TSX multi-server setup -*- lexical-binding: t; -*-

;; This file specifies three language servers for TypeScript and TSX.  Eglot
;; connects one server for each major mode and project.  The Rassumfrassum
;; multiplexer (`rass') therefore runs the servers together:
;;
;;   typescript-language-server + eslint   (the bundled `tslint' preset)
;;   tailwindcss-language-server           (added after `--')
;;
;; ESLint moves out of the Flycheck `javascript-eslint' checker.  That checker
;; runs `eslint --format=json --stdin' as a subprocess.  Flycheck starts a check
;; on `idle-change' and on `new-line' by default.  A type-aware
;; typescript-eslint configuration makes each run take approximately 6 seconds.
;; Flycheck then cancels each check and starts it again, so few checks finish.
;; The language server keeps one warm process instead.
;;
;; The `javascript-eslint' checker stays registered.  The user starts Eglot
;; manually with `M-x eglot'.  The predicate of `eglot-check' requires
;; `eglot-managed-p'.  In a TS buffer without a connection, Flycheck therefore
;; selects the CLI checker.  This is the fallback path.  For the
;; `flycheck-eglot-exclusive' half, see flycheck-migration-test.el.  That
;; variable stops the two sources from reporting each result twice.
;;
;; The fallback path also gets a check-trigger limit.  Flycheck starts a check
;; on `idle-change' and on `new-line' by default.  Each CLI run takes
;; approximately 6 seconds, so the buffer-local value keeps only `save' and
;; `mode-enabled'.  This limit is safe for the LSP path.
;; `flycheck-eglot--report' calls `flycheck-buffer-automatically' with no
;; condition, and `flycheck-may-check-automatically' then returns non-nil for
;; any trigger list.
;;
;; This file pins down two failures that are easy to miss:
;;
;; * The entry must be explicit.  Do not use `eglot-alternatives'.  Eglot 1.24
;;   offers `("rass" "ts")' for these modes.  Eglot tests only for the `rass'
;;   program.  Rassumfrassum 0.3.4 has no `ts' preset.  It has tslint, tsbiome,
;;   tyruff, basedruff and vuetail.  If `rass' is on the PATH, Eglot selects a
;;   command that stops immediately.
;;
;; * Each mode must declare a `:language-id'.  If the ID is absent, Eglot
;;   removes "\\(?:-ts\\)?-mode$" from the name of the mode.  This gives "tsx"
;;   for `tsx-ts-mode' and "js" for `js-ts-mode'.  These are not LSP language
;;   IDs.  Both servers use the ID.  The `validate: probe' option of ESLint and
;;   the class completion of tailwindcss-language-server then do nothing.
;;
;; The structural tests read the tangled config.el and flake.nix.  They run
;; anywhere.  The behavioural tests need the full package set and the loaded
;; configuration.  They test for `my/add-node-modules-path' on
;; `tsx-ts-mode-hook'.  The configuration always adds this function at load
;; time.  If the function is present, the configuration is loaded.  If it is
;; absent, this is the isolated emacs-nox run.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defconst ems-test--ts-modes '(tsx-ts-mode typescript-ts-mode js-ts-mode)
  "The tree-sitter TS and JS modes that the multiplexer entry must cover.")

(defconst ems-test--language-ids
  '((tsx-ts-mode . "typescriptreact")
    (typescript-ts-mode . "typescript")
    (js-ts-mode . "javascript"))
  "The LSP language ID that each mode must declare.")

(defun ems-test--config-loaded-p ()
  "Return non-nil if this Emacs has loaded the full configuration."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

(defun ems-test--server-program-entries ()
  "Return each entry that config.el adds to `eglot-server-programs'.
An entry is the (MODES . CONTACT) cons from an `add-to-list' call."
  (let (entries)
    (dolist (form (cfg-test-read-forms))
      (dolist (call (cfg-test-find-all form 'add-to-list))
        (when (equal (nth 1 call) ''eglot-server-programs)
          (let ((entry (nth 2 call)))
            ;; This configuration uses two shapes.  A quoted literal, and
            ;; `(cons (quote (MODES)) (lambda ...))' for the Markdown resolver.
            (push (cond
                   ((and (consp entry) (eq (car entry) 'quote)) (cadr entry))
                   ((and (consp entry) (eq (car entry) 'cons))
                    (cons (cadr (nth 1 entry)) (nth 2 entry)))
                   (t entry))
                  entries)))))
    (nreverse entries)))

(defun ems-test--mode-specs (entry)
  "Return the mode specifications of ENTRY as a list.
This function also normalises the single-mode shape."
  (let ((modes (car entry)))
    (cond
     ((not (consp modes)) (list modes))
     ;; (MODE :language-id "x") is one specification, not a list of modes.
     ((memq :language-id modes) (list modes))
     (t modes))))

(defun ems-test--spec-mode (spec)
  "Return the major-mode symbol of the mode specification SPEC."
  (if (consp spec) (car spec) spec))

(defun ems-test--ts-entry ()
  "Return the `eglot-server-programs' entry of config.el for `tsx-ts-mode'."
  (cl-find-if (lambda (entry)
                (memq 'tsx-ts-mode
                      (mapcar #'ems-test--spec-mode
                              (ems-test--mode-specs entry))))
              (ems-test--server-program-entries)))

(defun ems-test--ts-command ()
  "Return the command that config.el uses for the TS and TSX modes."
  (cdr (ems-test--ts-entry)))

(defun ems-test--setq-local-value (var)
  "Return the value that a `setq-local' in config.el gives VAR, or nil.
The value is unquoted."
  (catch 'found
    (dolist (form (cfg-test-read-forms))
      (dolist (call (cfg-test-find-all form 'setq-local))
        (let ((tail (cdr call)))
          ;; `setq-local' takes pairs, so step through them two at a time.
          (while tail
            (when (eq (car tail) var)
              (let ((value (cadr tail)))
                (throw 'found
                       (if (and (consp value) (eq (car value) 'quote))
                           (cadr value)
                         value))))
            (setq tail (cddr tail))))))
    nil))

(defun ems-test--emacs-tools ()
  "Return the package names in the `emacsToolsFor' list of flake.nix."
  (with-temp-buffer
    (insert-file-contents "flake.nix")
    (goto-char (point-min))
    (re-search-forward "emacsToolsFor[[:space:]]*=")
    (re-search-forward "\\[")
    (let ((start (point)))
      (re-search-forward "\\]")
      (split-string (buffer-substring-no-properties start (1- (point))) nil t))))

;;; Structural -- the tangled configuration and the tool closure

(ert-deftest eglot-multiserver/ts-modes-get-an-explicit-entry ()
  "config.el adds one `eglot-server-programs' entry for the three TS modes."
  ;; Arrange / Act
  (let ((entry (ems-test--ts-entry)))
    ;; Assert
    (should entry)
    (let ((modes (mapcar #'ems-test--spec-mode (ems-test--mode-specs entry))))
      (dolist (mode ems-test--ts-modes)
        (should (memq mode modes))))))

(ert-deftest eglot-multiserver/command-runs-the-multiplexer ()
  "The TS and TSX contact is a `rass' command, not a single language server."
  ;; Arrange / Act
  (let ((command (ems-test--ts-command)))
    ;; Assert
    (should (consp command))
    (should (equal (car command) "rass"))))

(ert-deftest eglot-multiserver/command-is-not-the-broken-builtin-alternative ()
  "The entry is explicit.  It is not `(\"rass\" \"ts\")' or an alternative.
Rassumfrassum 0.3.4 has no `ts' preset.  The built-in alternative of Eglot
therefore stops immediately."
  ;; Arrange / Act
  (let ((command (ems-test--ts-command))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert -- test for the command first, or the tests below all pass on nil.
    (should (consp command))
    (should-not (equal command '("rass" "ts")))
    (should-not (member "ts" command))
    (should-not (string-match-p "eglot-alternatives" code))))

(ert-deftest eglot-multiserver/typescript-eslint-and-tailwind-all-served ()
  "The command starts the three servers.  `tslint' gives typescript-ls and eslint."
  ;; Arrange / Act
  (let ((command (ems-test--ts-command)))
    ;; Assert
    (should (member "tslint" command))
    (should (member "tailwindcss-language-server" command))
    ;; The added server needs the `--' separator as its own argument.
    (should (member "--" command))
    (should (< (cl-position "tslint" command :test #'equal)
               (cl-position "--" command :test #'equal)))))

(ert-deftest eglot-multiserver/language-ids-are-explicit ()
  "Each mode declares its LSP language ID.
The Eglot default gives \"tsx\" for `tsx-ts-mode' and \"js\" for `js-ts-mode'.
The `validate: probe' option of ESLint and the Tailwind class completion then
do nothing."
  ;; Arrange / Act
  (let ((specs (ems-test--mode-specs (ems-test--ts-entry))))
    ;; Assert
    (pcase-dolist (`(,mode . ,id) ems-test--language-ids)
      (let ((spec (cl-find mode specs :key #'ems-test--spec-mode)))
        (should (consp spec))
        (should (equal (plist-get (cdr spec) :language-id) id))))))

(ert-deftest eglot-multiserver/tools-are-in-the-closure ()
  "flake.nix puts the multiplexer and the two new servers on the PATH."
  ;; Arrange / Act
  (let ((tools (ems-test--emacs-tools)))
    ;; Assert
    (should (member "rassumfrassum" tools))
    (should (member "tailwindcss-language-server" tools))
    ;; This package gives `vscode-eslint-language-server'.  The tslint preset
    ;; looks for that program first.
    (should (member "vscode-langservers-extracted" tools))))

(ert-deftest eglot-multiserver/eslint-fallback-checks-on-save-only ()
  "In a TS buffer, Flycheck starts a check on `save' but not while you type.
This limit applies to the fallback path.  Without a connection, Flycheck
selects `javascript-eslint', and each run takes approximately 6 seconds.  The
Flycheck default also starts a check on `idle-change' and on `new-line'.
Flycheck then cancels each slow run and starts it again, so few runs finish."
  ;; Arrange / Act
  (let ((triggers (ems-test--setq-local-value 'flycheck-check-syntax-automatically)))
    ;; Assert
    (should triggers)
    (should (memq 'save triggers))
    (should-not (memq 'idle-change triggers))
    (should-not (memq 'new-line triggers))))

(ert-deftest eglot-multiserver/check-trigger-limit-is-buffer-local ()
  "The limit uses `setq-local' and stays out of a `:custom' block.
A global value would also slow the Nix and Elisp checkers.  Those checkers are
fast, and live results are useful there."
  ;; Arrange / Act -- the helper reads `setq-local' forms only, so a value here
  ;; proves the assignment is buffer-local.
  (should (ems-test--setq-local-value 'flycheck-check-syntax-automatically))
  ;; Assert -- a `:custom' entry would make the value global.
  (dolist (form (cfg-test-read-forms))
    (dolist (up (cfg-test-find-all form 'use-package))
      (let ((tail (cdr (memq :custom up))))
        (while (and tail (not (keywordp (car tail))))
          (should-not (eq (car-safe (car tail))
                          'flycheck-check-syntax-automatically))
          (setq tail (cdr tail)))))))

;;; Behavioural -- the real package set with the loaded configuration

(ert-deftest eglot-multiserver/lookup-resolves-to-the-multiplexer ()
  "The Eglot lookup selects the configuration entry, not the built-in mapping."
  ;; Arrange
  (skip-unless (ems-test--config-loaded-p))
  (should (require 'eglot nil t))
  (skip-unless (fboundp 'eglot--lookup-mode))
  ;; Act -- the function returns (LANGUAGES . CONTACT-PROXY).
  (let ((contact (cdr (eglot--lookup-mode 'tsx-ts-mode))))
    ;; Assert
    (should (equal (car contact) "rass"))
    (should (member "tailwindcss-language-server" contact))))

(ert-deftest eglot-multiserver/lookup-language-ids-are-lsp-names ()
  "The language IDs are the LSP names, not the Eglot default from the mode name."
  ;; Arrange
  (skip-unless (ems-test--config-loaded-p))
  (should (require 'eglot nil t))
  (skip-unless (fboundp 'eglot--lookup-mode))
  ;; Act / Assert
  (pcase-dolist (`(,mode . ,id) ems-test--language-ids)
    (let ((languages (car (eglot--lookup-mode mode))))
      (should (equal (alist-get mode languages nil nil #'eq) id)))))

(ert-deftest eglot-multiserver/eslint-checker-stays-for-unmanaged-buffers ()
  "The `javascript-eslint' checker stays as the fallback without a connection.
The user starts Eglot manually.  Flycheck selects `eglot-check' only in a
buffer that Eglot manages.  Flycheck must therefore still reach the CLI
checker in the other buffers."
  ;; Arrange
  (skip-unless (ems-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Assert
  (should (memq 'javascript-eslint flycheck-checkers))
  (dolist (mode ems-test--ts-modes)
    (should (flycheck-checker-supports-major-mode-p 'javascript-eslint mode)))
  ;; This predicate makes the fallback work.  Flycheck wraps a `:predicate' in a
  ;; closure that binds `default-directory' first, so test the behaviour of the
  ;; predicate.  Do not compare it with `flycheck-eglot--enabled-p' by identity.
  (let ((predicate (flycheck-checker-get 'eglot-check 'predicate)))
    (should (functionp predicate))
    (with-temp-buffer
      (rename-buffer "eslint-fallback-test.tsx" t)
      ;; Eglot does not manage this buffer, so Flycheck must refuse
      ;; `eglot-check' and fall through to `javascript-eslint'.
      (should-not (funcall predicate)))))

(ert-deftest eglot-multiserver/trigger-limit-does-not-delay-lsp-diagnostics ()
  "The trigger limit does not stop live LSP results in a managed buffer.
`flycheck-eglot--report' calls `flycheck-buffer-automatically' with no
condition.  `flycheck-may-check-automatically' then returns non-nil for any
value of `flycheck-check-syntax-automatically'.  Eglot therefore publishes each
diagnostic immediately, and the limit throttles only the CLI checker."
  ;; Arrange
  (skip-unless (ems-test--config-loaded-p))
  (should (require 'flycheck nil t))
  ;; Act / Assert
  (with-temp-buffer
    ;; `flycheck-may-check-automatically' excludes an ephemeral buffer by name.
    (rename-buffer "ts-trigger-limit-test.tsx" t)
    (let ((flycheck-check-syntax-automatically '(save)))
      ;; The Eglot report path passes no condition.
      (should (flycheck-may-check-automatically))
      ;; A typing trigger is refused, which is the point of the limit.
      (should-not (flycheck-may-check-automatically 'idle-change))
      (should-not (flycheck-may-check-automatically 'new-line))
      ;; `mode-enabled' is refused too, so a Consult preview costs nothing.
      (should-not (flycheck-may-check-automatically 'mode-enabled))
      (should (flycheck-may-check-automatically 'save)))))

(provide 'eglot-multiserver-test)
;;; eglot-multiserver-test.el ends here
