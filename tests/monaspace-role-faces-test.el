;;; monaspace-role-faces-test.el --- Tests for my/apply-monaspace-role-faces -*- lexical-binding: t; -*-

;; `my/apply-monaspace-role-faces' re-applies the Monaspace "voice" faces on
;; every catppuccin flavour switch, because `catppuccin-reload' resets face
;; specs.  The italic faces (base `italic', comment, keyword) must be part of
;; that set: they are theme-restyled, so if they are only set once at top level
;; the reload wipes them and italics disappear everywhere.  This test pins that
;; the helper points those faces at Monaspace Radon with an italic slant.
;;
;; It also pins the absence of `ligature'.  That package worked -- composition
;; applied, `find-composition' returned non-nil -- but no ligature ever rendered,
;; because Monaspace keeps its programming ligatures in stylistic sets that are
;; off by default:
;;
;;   ss01 Equal Symbols   ss03 Arrows        ss06 Markdown Strings
;;   ss02 Comparisons     ss04 HTML Tags     ss09 Double Arrows   ...
;;
;; Emacs cannot enable an OpenType feature on this NS build, so the sets stay
;; off.  FiraCode renders ligatures in the same Emacs because theirs live in
;; `calt', which is on by default -- that comparison is what isolated the cause
;; to the font rather than to Emacs.  Freezing the sets into the font is not an
;; option either: `pyftfeatfreeze' handles "only single and alternate
;; substitutions", and a ligature is many-to-one.
;;
;; Monaspace's texture healing is `calt' and is unaffected, so nothing was lost
;; by removing the package -- only a ~20 line ligature list that did nothing.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest monaspace-role-faces/applies-radon-italics ()
  "The flavour-hook helper points the italic faces at Radon, italic slant."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-monaspace-role-faces)
  ;; Act
  (my/apply-monaspace-role-faces)
  ;; Assert
  (dolist (face '(italic font-lock-comment-face font-lock-keyword-face))
    (should (equal (face-attribute face :family) "MonaspiceRn Nerd Font"))
    (should (eq (face-attribute face :slant) 'italic))))

(ert-deftest monaspace-role-faces/reapplies-base-default-font ()
  "The helper re-applies the base default font size and weight.
`catppuccin-reload' re-specs the (themed) `default' face and strips these, so
they must be re-applied from the flavour hook, not just once at top level."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-monaspace-role-faces)
  ;; Simulate a post-reload `default' face with height/weight stripped.
  (set-face-attribute 'default nil :height 100 :weight 'normal)
  ;; Act
  (my/apply-monaspace-role-faces)
  ;; Assert
  (should (eq (face-attribute 'default :height) 140))
  (should (eq (face-attribute 'default :weight) 'medium)))

(ert-deftest monaspace-role-faces/ligature-package-is-not-wired ()
  "`ligature' stays out: Monaspace gates its ligatures behind stylistic sets.
See this file's header.  Both halves must go, or the closure keeps a package the
configuration never loads."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (member "ligature" packages))
    (should-not (string-match-p "ligature" code))))

(provide 'monaspace-role-faces-test)
;;; monaspace-role-faces-test.el ends here
