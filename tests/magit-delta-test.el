;;; magit-delta-test.el --- Tests for the magit-delta setup -*- lexical-binding: t; -*-

;; `magit-delta' pipes Magit's diffs through `delta', which does its own
;; syntax-aware highlighting.  Two things have to hold for that to work.
;;
;; The `delta' program must be in the Nix tool closure.  `magit-delta' shells
;; out to whatever `magit-delta-delta-executable' names, and the default is a
;; bare "delta", so an absent binary is a runtime failure rather than a load
;; error.
;;
;; The syntax theme must match the active Catppuccin flavor.  `magit-delta'
;; chooses between `magit-delta-default-light-theme' and
;; `magit-delta-default-dark-theme' from the frame's `background-mode'.  Its own
;; defaults are "GitHub" and "Monokai Extended", which clash with Catppuccin.
;; delta 0.19.2 has "Catppuccin Latte" and "Catppuccin Mocha" built in, and those
;; are the same two flavors `my/catppuccin-flavor-for' selects, so the tests
;; below tie the two together and stop them drifting apart.
;;
;; Structural only: these parse the tangled config.el and flake.nix, so they run
;; anywhere.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun md-test--use-package-form ()
  "Return the `use-package magit-delta' form from config.el, or nil."
  (catch 'found
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (when (eq (nth 1 up) 'magit-delta)
          (throw 'found up))))
    nil))

(defun md-test--custom-value (var)
  "Return the value the `use-package magit-delta' `:custom' block gives VAR."
  (let ((tail (cdr (memq :custom (md-test--use-package-form))))
        (result 'unset))
    (while (and tail (not (keywordp (car tail))))
      (when (and (consp (car tail)) (eq (car (car tail)) var))
        (setq result (cadr (car tail))))
      (setq tail (cdr tail)))
    result))

(ert-deftest magit-delta/package-and-binary-are-in-the-closure ()
  "flake.nix supplies both halves: the ELisp package and the `delta' program."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
        (tools (cfg-test-nix-list "emacsToolsFor")))
    ;; Assert
    (should (member "magit-delta" packages))
    (should (member "delta" tools))))

(ert-deftest magit-delta/enabled-in-magit-buffers ()
  "The mode is hooked to `magit-mode' and loads after Magit."
  ;; Arrange / Act
  (let* ((form (md-test--use-package-form))
         (printed (prin1-to-string form)))
    ;; Assert
    (should form)
    (should (eq (cadr (memq :after form)) 'magit))
    (should (string-match-p "magit-mode" printed))
    (should (string-match-p "magit-delta-mode" printed))))

(ert-deftest magit-delta/syntax-themes-track-the-catppuccin-flavors ()
  "The delta themes are the Catppuccin ones, and match `my/catppuccin-flavor-for'.
Both are built into delta 0.19.2.  The light/dark split has to agree with the
flavor function, or a light Emacs frame gets a dark diff."
  ;; Arrange
  (cfg-test-load-defun 'my/catppuccin-flavor-for)
  ;; Act
  (let ((light (md-test--custom-value 'magit-delta-default-light-theme))
        (dark (md-test--custom-value 'magit-delta-default-dark-theme)))
    ;; Assert
    (should (equal light "Catppuccin Latte"))
    (should (equal dark "Catppuccin Mocha"))
    ;; The flavor function must agree about which is which.
    (should (string-match-p (symbol-name (my/catppuccin-flavor-for 'light))
                            (downcase light)))
    (should (string-match-p (symbol-name (my/catppuccin-flavor-for 'dark))
                            (downcase dark)))))

(provide 'magit-delta-test)
;;; magit-delta-test.el ends here
