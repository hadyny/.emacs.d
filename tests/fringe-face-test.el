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
;; this needs no "not yet loaded" lifecycle path.

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

(provide 'fringe-face-test)
;;; fringe-face-test.el ends here
