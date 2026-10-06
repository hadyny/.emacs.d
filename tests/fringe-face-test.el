;;; fringe-face-test.el --- Tests for my/apply-fringe-face -*- lexical-binding: t; -*-

;; A theme can give `fringe' its own colour, a shade apart from `default's
;; background -- fine at the window edge, but Flycheck's indicators sit in the
;; *margin*, immediately to the right of the fringe (see the Flycheck
;; section), where the two backgrounds abutted and the seam fell right where
;; the error/warning glyphs are read. `my/apply-fringe-face' levels `fringe'
;; to `default''s resolved background instead, called from the appearance hook
;; alongside `my/apply-diff-hl-faces' so it tracks Latte/Mocha rather than
;; being pinned to one palette.
;;
;; It reads `default' rather than `catppuccin-color': that lookup always
;; returns the GUI colour, but on a 256-colour terminal Catppuccin draws
;; `default' with a quantised one, so the two would not match there.
;;
;; `fringe' is a built-in face that always exists, unlike diff-hl's faces, so
;; that half needs no "not yet loaded" lifecycle path. `margin' is Emacs 31's
;; new basic face for margin display strings -- left unlevelled, it can differ
;; from `fringe', which is what caused the seam in the first place -- and
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

(defmacro fringe-face-test--with-default-background (color &rest body)
  "Run BODY with `default''s background set to COLOR, then restore it.
Faces are global to the process, so a leaked background would change what
later tests in the same batch run see."
  (declare (indent 1))
  `(let ((saved (face-attribute 'default :background)))
     (unwind-protect
         (progn
           (set-face-attribute 'default nil :background ,color)
           ,@body)
       (set-face-attribute 'default nil :background saved))))

(ert-deftest fringe-face/matches-bg ()
  "`fringe's background is levelled to `default''s background."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-fringe-face)
  (fringe-face-test--with-default-background "#1a1a1a"
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
  (fringe-face-test--with-default-background "#1a1a1a"
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

(ert-deftest fringe-face/margin-matches-bg-when-defined ()
  "On Emacs 31+, `margin's background is levelled to `default''s background.
`fringe-face/margin-tracks-emacs-31-availability' skips before its own
assertion whenever `margin' already exists, so on Emacs 31 this is the test
that checks the colour."
  (skip-unless (facep 'margin))
  ;; Arrange
  (cfg-test-load-defun 'my/apply-fringe-face)
  (fringe-face-test--with-default-background "#2b2b3c"
    ;; Act
    (my/apply-fringe-face)
    ;; Assert
    (should (equal (face-attribute 'margin :background) "#2b2b3c"))))

(provide 'fringe-face-test)
;;; fringe-face-test.el ends here
