;;; diff-hl-faces-test.el --- Tests for my/apply-diff-hl-faces -*- lexical-binding: t; -*-

;; `my/apply-diff-hl-faces' recolours diff-hl's fringe/margin faces from the
;; active Tokyo Night variant, via the package's `tokyo-night-get-color' palette
;; lookup.  It is called on every variant switch, including the initial one
;; applied at startup by the `auto-dark' block -- which runs *before* the
;; `:defer'red diff-hl package has loaded and defined its faces.  So the function
;; must be safe to call when `diff-hl-change' et al. do not yet exist, and must
;; actually colour them once they do.
;;
;; Faces are global to the Emacs process and cannot be un-defined, so this is a
;; single lifecycle test rather than two: it exercises the "not yet loaded" path
;; (real absence -- batch `emacs -Q' never loads diff-hl) *before* creating the
;; faces, then the "loaded" path.  That keeps it independent of ERT's test
;; ordering.
;;
;; `tokyo-night-get-color' takes a *string* palette key ("tokyo-blue"), unlike
;; `catppuccin-color', which took a symbol.  The stub below holds the call to that
;; contract: a symbol argument would return nil against the real package and the
;; faces would silently lose their colour.

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
Tokyo Night foreground."
  ;; Arrange -- the real Night palette, keyed as `tokyo-night-get-color' keys it.
  (cfg-test-load-defun 'my/apply-diff-hl-faces)
  (cl-letf (((symbol-function 'tokyo-night-get-color)
             (lambda (name &optional _theme)
               (should (stringp name))
               (cdr (assoc name '(("tokyo-blue"  . "#7aa2f7")
                                  ("tokyo-red"   . "#f7768e")
                                  ("tokyo-green" . "#9ece6a")))))))
    ;; Act / Assert -- startup: diff-hl not loaded, faces genuinely absent.
    (should-not (facep 'diff-hl-change))
    (should (progn (my/apply-diff-hl-faces) t))
    ;; Arrange -- diff-hl loads and defines its faces.
    (make-face 'diff-hl-change)
    (make-face 'diff-hl-delete)
    (make-face 'diff-hl-insert)
    ;; Act
    (my/apply-diff-hl-faces)
    ;; Assert -- each face now carries the Tokyo Night foreground.
    (should (equal (face-attribute 'diff-hl-change :foreground) "#7aa2f7"))
    (should (equal (face-attribute 'diff-hl-delete :foreground) "#f7768e"))
    (should (equal (face-attribute 'diff-hl-insert :foreground) "#9ece6a"))))

(provide 'diff-hl-faces-test)
;;; diff-hl-faces-test.el ends here
