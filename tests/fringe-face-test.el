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
;; is guarded by `facep' in `my/apply-fringe-face' for Emacs <31, where it
;; does not exist yet. The CI checks' `emacs-nox' has since moved to Emacs 31,
;; where `margin' is built in and already present at startup, so the "not yet
;; loaded" half of the test below is skipped rather than run against a stub.

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
Faces are global to the process and cannot be un-defined, so the \"does not
exist yet\" path (real absence: batch `emacs -Q' on Emacs 30 never defines it)
only gets exercised on an Emacs old enough to lack `margin' -- the CI checks'
`emacs-nox' has moved to Emacs 31, where the face is built in and already
present at startup, so that half is skipped rather than run against a stub;
the same reason `apply-diff-hl-faces/tracks-diff-hl-load-lifecycle' guards
its \"not yet loaded\" half on package state instead of a version check."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-fringe-face)
  (cl-letf (((symbol-function 'modus-themes-get-color-value)
             (lambda (name &optional _with-overrides _theme)
               (should (eq name 'bg-main))
               "#1a1a1a")))
    ;; Act / Assert -- startup: `margin' does not exist yet (Emacs <31 only).
    (if (facep 'margin)
        (ert-skip "`margin' is already defined on this Emacs (31+)")
      (progn
        (should-not (facep 'margin))
        (should (progn (my/apply-fringe-face) t))))
    ;; Arrange -- Emacs 31 defines the basic `margin' face.
    (unless (facep 'margin) (make-face 'margin))
    ;; Act
    (my/apply-fringe-face)
    ;; Assert
    (should (equal (face-attribute 'margin :background) "#1a1a1a"))))

(provide 'fringe-face-test)
;;; fringe-face-test.el ends here
