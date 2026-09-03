;;; font-faces-test.el --- Tests for my/apply-font-faces -*- lexical-binding: t; -*-

;; Code is Maple Mono NF: `default', `fixed-pitch', the fallback glyph face and
;; the nerd-icons family.  Prose (`variable-pitch') is Inter, chosen on x-height
;; -- 0.546em against Maple Mono's 0.550em -- so both look the same size at one
;; `:height' and `mixed-pitch' needs no fudge factor.
;;
;; With one family for code, no face needs its own: an unspecified family falls
;; through to `default' at render time.  Two things do not survive/exist after
;; the `load-theme' of a Doom Themes variant and must be (re-)applied from the
;; appearance hook, the same reason `my/apply-diff-hl-faces' exists:
;;
;;   * `default' is themed (fg/bg), so a reload strips its family/height/weight;
;;   * Doom Dracula and Doom Solarized Light disagree on which font-lock faces
;;     get emphasis at all -- Solarized Light slants comments/types/builtins
;;     and bolds keywords/constants, Dracula styles none of them.
;;     `doom-themes-enable-bold'/`doom-themes-enable-italic' only *permit* a
;;     variant's own emphasis, they do not add any where a variant specifies
;;     none, so Solarized Light's choices are forced onto both here -- code
;;     emphasis would otherwise vanish entirely under Dracula.
;;
;; Ligatures: Maple Mono's `calt' can draw its arrows without composition, but
;; in practice that did not render here, so `ligature.el' composes the pair
;; instead -- see the Ligatures section in config.org.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest font-faces/reapplies-base-default-font ()
  "The helper re-applies the base default font size and weight.
`load-theme' re-specs the (themed) `default' face and strips these, so they must
be re-applied from the appearance hook, not just once at top level."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-font-faces)
  ;; Simulate a post-reload `default' face with height/weight stripped.
  (set-face-attribute 'default nil :height 100 :weight 'normal)
  ;; Act
  (my/apply-font-faces)
  ;; Assert
  (should (eq (face-attribute 'default :height) 140))
  (should (eq (face-attribute 'default :weight) 'medium)))

(ert-deftest font-faces/forces-solarized-lights-emphasis-on-both-variants ()
  "Comment/type/builtin faces are italic; keyword/constant are bold.
This must not depend on which variant is currently loaded -- Dracula
specifies none of this itself, so the helper has to force it rather than
merely permit it via `doom-themes-enable-bold'/`doom-themes-enable-italic'."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-font-faces)
  (dolist (face '(font-lock-comment-face font-lock-type-face
                  font-lock-builtin-face))
    (set-face-attribute face nil :slant 'normal))
  (dolist (face '(font-lock-keyword-face font-lock-constant-face))
    (set-face-attribute face nil :weight 'normal))
  ;; Act
  (my/apply-font-faces)
  ;; Assert
  (dolist (face '(font-lock-comment-face font-lock-type-face
                  font-lock-builtin-face))
    (should (eq (face-attribute face :slant) 'italic)))
  (dolist (face '(font-lock-keyword-face font-lock-constant-face))
    (should (eq (face-attribute face :weight) 'bold))))

(ert-deftest font-faces/code-is-maple-mono-prose-is-inter ()
  "Code faces are Maple Mono NF; `variable-pitch' is Inter."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should (string-match-p "set-face-attribute 'default nil :font \"Maple Mono NF\"" code))
    (should (string-match-p "nerd-icons-font-family \"Maple Mono NF\"" code))
    (should (string-match-p "'variable-pitch nil[[:space:]]+:font \"Inter\"" code))
    ;; `fixed-pitch' -- code inside prose buffers -- must stay with the code font.
    (should (string-match-p "'fixed-pitch nil[[:space:]]+:font \"Maple Mono NF\"" code))
    (should-not (string-match-p "Monaspice" code))))

(ert-deftest font-faces/no-face-carries-a-redundant-family ()
  "`my/apply-font-faces' sets a family only on `default'.
With one typeface everywhere, a per-face `:family' is dead weight that has to be
found and changed again next time the font moves."
  ;; Arrange / Act
  (let* ((defun-form (cfg-test-find-defun (cons 'progn (cfg-test-read-forms))
                                          'my/apply-font-faces))
         (printed (prin1-to-string defun-form)))
    ;; Assert -- `default' gets `:font' (family plus size and weight in one);
    ;; nothing else should name a family at all.
    (should defun-form)
    (should (string-match-p ":font \"Maple Mono NF\"" printed))
    (should-not (string-match-p ":family" printed))))

(ert-deftest font-faces/ligature-package-is-wired ()
  "`ligature' is installed and configured for `prog-mode'.
Maple Mono's `calt' did not render its ligatures here in practice, so
`ligature.el' composes them instead."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
        (configured (let (names)
                      (dolist (form (cfg-test-read-forms))
                        (dolist (up (cfg-test-find-all form 'use-package))
                          (push (nth 1 up) names)))
                      names)))
    ;; Assert
    (should (member "ligature" packages))
    (should (memq 'ligature configured))))

(ert-deftest font-faces/no-top-level-face-reads ()
  "No top-level form reads a realised face value.
`(face-attribute ...)' at load time is answered before the theme has been
applied -- and in a daemon before any frame exists at all -- so anything derived
from it is pinned to the wrong colour until the next variant switch re-specs the
face.  Faces the theme owns must be left to the theme; anything that genuinely
needs to read one belongs in a function called from the appearance hook, like
`my/apply-diff-hl-faces'.

This caught `(set-face-background \='fringe (face-attribute \='default
:background))', which produced a mis-coloured fringe in the first client frame
of an Emacs daemon.  It was redundant too: the theme already gives `fringe' the
same background as `default'."
  ;; Arrange / Act
  (let (offenders)
    (dolist (form (cfg-test-read-forms))
      (unless (memq (car-safe form) '(defun use-package with-eval-after-load))
        (let ((printed (prin1-to-string form)))
          ;; "(face-attribute" does not match "(set-face-attribute" -- the
          ;; character before `face-' is `-', not `('.
          (when (string-match-p "(\\(face-attribute\\|face-background\\|face-foreground\\)"
                                printed)
            (push (substring printed 0 (min 70 (length printed))) offenders)))))
    ;; Assert
    (should (null offenders))))

(provide 'font-faces-test)
;;; font-faces-test.el ends here
