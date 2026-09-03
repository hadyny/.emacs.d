;;; diff-hl-faces-test.el --- Tests for my/apply-diff-hl-faces -*- lexical-binding: t; -*-

;; `my/apply-diff-hl-faces' recolours diff-hl's fringe/margin faces from the
;; active Doom Themes variant, via the package's `doom-color' palette lookup.
;; It is called on every variant switch, including the initial one applied at
;; startup by the `auto-dark' block -- which runs *before* the `:defer'red
;; diff-hl package has loaded and defined its faces.  So the function must be
;; safe to call when `diff-hl-change' et al. do not yet exist, and must
;; actually colour them once they do.
;;
;; Faces are global to the Emacs process and cannot be un-defined, so this is a
;; single lifecycle test rather than two: it exercises the "not yet loaded" path
;; (real absence -- batch `emacs -Q' never loads diff-hl) *before* creating the
;; faces, then the "loaded" path.  That keeps it independent of ERT's test
;; ordering.
;;
;; `doom-color' takes a *symbol* palette key (`vc-added'), unlike
;; `tokyo-night-get-color', which took a string.  The stub below holds the
;; call to that contract: a string argument would return nil against the real
;; package and the faces would silently lose their colour.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest apply-diff-hl-faces/tracks-diff-hl-load-lifecycle ()
  "No-op before diff-hl defines its faces; colours them once it has.
The startup path (auto-dark applies the variant before the deferred diff-hl
loads) must not signal \"Invalid face\", and once the faces exist each takes its
Doom Themes foreground."
  ;; Arrange -- a stub palette, keyed as `doom-color' keys it.
  (cfg-test-load-defun 'my/apply-diff-hl-faces)
  (cl-letf (((symbol-function 'doom-color)
             (lambda (name &optional _type)
               (should (symbolp name))
               (cdr (assq name '((vc-added    . "#a0e0a0")
                                 (vc-modified . "#efef80")
                                 (vc-deleted  . "#ffbfbf")))))))
    ;; Act / Assert -- startup: diff-hl not loaded, faces genuinely absent.
    (should-not (facep 'diff-hl-change))
    (should (progn (my/apply-diff-hl-faces) t))
    ;; Arrange -- diff-hl loads and defines its faces.
    (make-face 'diff-hl-change)
    (make-face 'diff-hl-delete)
    (make-face 'diff-hl-insert)
    ;; Act
    (my/apply-diff-hl-faces)
    ;; Assert -- each face now carries the Doom Themes foreground.
    (should (equal (face-attribute 'diff-hl-change :foreground) "#efef80"))
    (should (equal (face-attribute 'diff-hl-delete :foreground) "#ffbfbf"))
    (should (equal (face-attribute 'diff-hl-insert :foreground) "#a0e0a0"))))

(provide 'diff-hl-faces-test)
;;; diff-hl-faces-test.el ends here
