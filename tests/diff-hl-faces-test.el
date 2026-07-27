;;; diff-hl-faces-test.el --- Tests for my/apply-diff-hl-faces -*- lexical-binding: t; -*-

;; `my/apply-diff-hl-faces' recolours diff-hl's fringe/margin faces from the
;; active catppuccin flavor.  It is called on every flavor switch, including the
;; initial one applied at startup by the `auto-dark' block -- which runs *before*
;; the `:defer'red diff-hl package has loaded and defined its faces.  So the
;; function must be safe to call when `diff-hl-change' et al. do not yet exist,
;; and must actually colour them once they do.
;;
;; Faces are global to the Emacs process and cannot be un-defined, so this is a
;; single lifecycle test rather than two: it exercises the "not yet loaded" path
;; (real absence -- batch `emacs -Q' never loads diff-hl) *before* creating the
;; faces, then the "loaded" path.  That keeps it independent of ERT's test
;; ordering.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest apply-diff-hl-faces/tracks-diff-hl-load-lifecycle ()
  "No-op before diff-hl defines its faces; colours them once it has.
The startup path (auto-dark applies the flavor before the deferred diff-hl
loads) must not signal \"Invalid face\", and once the faces exist each takes
its catppuccin foreground."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-diff-hl-faces)
  (cl-letf (((symbol-function 'catppuccin-color)
             (lambda (name)
               (pcase name
                 ('blue "#1e66f5")
                 ('red "#d20f39")
                 ('green "#40a02b")))))
    ;; Act / Assert -- startup: diff-hl not loaded, faces genuinely absent.
    (should-not (facep 'diff-hl-change))
    (should (progn (my/apply-diff-hl-faces) t))
    ;; Arrange -- diff-hl loads and defines its faces.
    (make-face 'diff-hl-change)
    (make-face 'diff-hl-delete)
    (make-face 'diff-hl-insert)
    ;; Act
    (my/apply-diff-hl-faces)
    ;; Assert -- each face now carries the catppuccin foreground.
    (should (equal (face-attribute 'diff-hl-change :foreground) "#1e66f5"))
    (should (equal (face-attribute 'diff-hl-delete :foreground) "#d20f39"))
    (should (equal (face-attribute 'diff-hl-insert :foreground) "#40a02b"))))

(provide 'diff-hl-faces-test)
;;; diff-hl-faces-test.el ends here
