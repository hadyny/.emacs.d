;;; fringe-face-test.el --- Tests for my/apply-fringe-face -*- lexical-binding: t; -*-

;; Modus Themes gives `fringe' its own colour, a shade apart from `default's
;; `bg-main' -- fine at the window edge, but Flycheck's indicators sit in the
;; *margin*, immediately to the right of the fringe (see the Flycheck
;; section), where the two backgrounds abutted and the seam fell right where
;; the error/warning glyphs are read. `my/apply-fringe-face' levels `fringe'
;; to `bg-main' instead, called from the appearance hook alongside
;; `my/apply-diff-hl-faces' so it tracks Operandi/Vivendi rather than being
;; pinned to one palette.
;;
;; `fringe' is a built-in face that always exists, unlike diff-hl's faces, so
;; that half needs no "not yet loaded" lifecycle path. `margin' is Emacs 31's
;; new basic face for margin display strings -- Modus Themes colours it the
;; same as `fringe', which is what caused the seam in the first place -- and
;; does not exist yet on the Emacs 30 `emacs-nox' the CI checks still build
;; tests against, so that half is guarded by `facep' and does need the
;; lifecycle treatment, the same reason `my/apply-diff-hl-faces' has one.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest fringe-face/matches-bg-main ()
  "`fringe's background is levelled to the palette's `bg-main'."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-fringe-face)
  (cl-letf (((symbol-function 'modus-themes-get-color-value)
             (lambda (name &optional _with-overrides _theme)
               (should (eq name 'bg-main))
               "#1a1a1a")))
    ;; Act
    (my/apply-fringe-face)
    ;; Assert
    (should (equal (face-attribute 'fringe :background) "#1a1a1a"))))

(ert-deftest fringe-face/margin-tracks-emacs-31-availability ()
  "`margin' is coloured once it exists (Emacs 31+); a no-op before that.
Faces are global to the process and cannot be un-defined, so this is a single
lifecycle test rather than two -- it exercises the \"does not exist yet\" path
(real absence: batch `emacs -Q' on Emacs 30 never defines it) before creating
the face, the same reason `apply-diff-hl-faces/tracks-diff-hl-load-lifecycle'
is structured this way."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-fringe-face)
  (cl-letf (((symbol-function 'modus-themes-get-color-value)
             (lambda (name &optional _with-overrides _theme)
               (should (eq name 'bg-main))
               "#1a1a1a")))
    ;; Act / Assert -- startup: `margin' genuinely does not exist (Emacs 30).
    (should-not (facep 'margin))
    (should (progn (my/apply-fringe-face) t))
    ;; Arrange -- Emacs 31 defines the basic `margin' face.
    (make-face 'margin)
    ;; Act
    (my/apply-fringe-face)
    ;; Assert
    (should (equal (face-attribute 'margin :background) "#1a1a1a"))))

(provide 'fringe-face-test)
;;; fringe-face-test.el ends here
