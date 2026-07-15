;;; keybindings-test.el --- Sanity checks for Evil keybindings -*- lexical-binding: t; -*-

;; Structural regression checks over the `evil-define-key' forms in config.el:
;;
;; * no two bindings in the same (state . map) target the same key sequence
;;   (a duplicate silently shadows the earlier binding) -- pure, parses the
;;   tangled config only;
;;
;; * every command a binding points at is `fboundp' once the full config is
;;   loaded (catches typos and renamed/removed commands that byte-compile does
;;   not flag). This half needs the real package set, so it self-skips when
;;   Evil is not loaded (e.g. a bare `emacs-nox' run).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun kb-test--key-string (key)
  "Return the string inside a (kbd \"...\") KEY form, or nil if not that shape."
  (when (and (consp key) (eq (car key) 'kbd) (stringp (cadr key)))
    (cadr key)))

(defun kb-test--def-symbol (def)
  "Return the command symbol from a quoted DEF like 'foo, or nil (e.g. lambdas)."
  (cond
   ((and (consp def) (eq (car def) 'quote) (symbolp (cadr def))) (cadr def))
   (t nil)))

(defun kb-test--bindings ()
  "Parse config.el; return a list of plists describing each Evil binding.
Each entry is (:state S :map M :key KEY-STRING :cmd SYMBOL-or-nil)."
  (let (result)
    (dolist (form (cfg-test-read-forms))
      (dolist (edk (cfg-test-find-all form 'evil-define-key))
        ;; (evil-define-key STATE MAP KEY DEF KEY DEF ...)
        (let ((state (nth 1 edk))
              (map   (nth 2 edk))
              (pairs (nthcdr 3 edk)))
          (while (cdr pairs)
            (push (list :state state
                        :map map
                        :key (kb-test--key-string (car pairs))
                        :cmd (kb-test--def-symbol (cadr pairs)))
                  result)
            (setq pairs (cddr pairs))))))
    (nreverse result)))

(ert-deftest keybindings/no-duplicate-chords ()
  "No (state . map) targets the same key sequence twice."
  ;; Arrange
  (let ((bindings (kb-test--bindings))
        (seen (make-hash-table :test 'equal))
        dupes)
    ;; Act
    (dolist (b bindings)
      (when (plist-get b :key)
        (let ((id (list (plist-get b :state) (plist-get b :map) (plist-get b :key))))
          (when (gethash id seen)
            (push id dupes))
          (puthash id t seen))))
    ;; Assert
    (should (null dupes))))

;; Features that own bound commands whose sub-commands are NOT autoloaded, so
;; the commands look "missing" until the feature loads. We require these
;; best-effort before checking so the test flags only genuinely undefined
;; commands (typos, or packages absent from the Nix closure) rather than
;; merely-unloaded ones. Deliberately minimal: packages whose bound commands
;; are already autoloaded (consult, magit, embark, ...) are omitted -- notably
;; `magit', whose load shells out to `git', absent in the Nix build sandbox.
(defconst kb-test--command-features
  '(flymake diff-hl eglot)
  "Features to `require' before the command-existence check.")

(ert-deftest keybindings/all-commands-fbound ()
  "Every command bound by an Evil binding is defined once the config is loaded."
  ;; Arrange: this needs the real package set + config defuns. Evil is
  ;; `:defer'red on `after-init', which never fires in batch, so key off the
  ;; autoloaded `evil-mode' (present once `package-activate-all' has run)
  ;; rather than `featurep'.
  (skip-unless (fboundp 'evil-mode))
  (dolist (feature kb-test--command-features)
    (require feature nil t))
  (let ((missing (cl-remove-if
                  (lambda (cmd) (or (null cmd) (fboundp cmd)))
                  (mapcar (lambda (b) (plist-get b :cmd)) (kb-test--bindings)))))
    ;; Assert
    (should (null missing))))

(provide 'keybindings-test)
;;; keybindings-test.el ends here
