;;; gnus-group-news-low-fix-test.el --- Tests for my/apply-gnus-group-news-low-fix -*- lexical-binding: t; -*-

;; Doom Themes (the build this flake currently pulls from nixpkgs) points
;; `gnus-group-news-low-empty' at `:inherit gnus-group-news-low' -- but Gnus's
;; own built-in spec for `gnus-group-news-low' inherits
;; `gnus-group-news-low-empty' right back.  Doom's face list also overrides
;; `gnus-group-news-low' itself (to inherit `gnus-group-mail-1' instead),
;; which should break the cycle, except a brand new child frame (Corfu's
;; popup) can recalculate one face of the pair before the other and briefly
;; hit Gnus's still-cyclic built-in spec, erroring instead of drawing.
;;
;; `my/apply-gnus-group-news-low-fix' repoints `gnus-group-news-low-empty' at
;; `gnus-group-mail-1-empty' instead -- upstream's own fix, which carries no
;; such cycle regardless of recalculation order -- by re-registering the
;; corrected spec on the variant theme itself with `custom-theme-set-faces'.
;; That distinction (over a plain `set-face-attribute') matters: only a spec
;; registered *on the enabled theme* survives the face recalculation any new
;; frame does against that theme's own registered spec table, which is why
;; these tests assert against a real `deftheme'/`enable-theme' rather than a
;; bare face, so a persistence regression back to `set-face-attribute' would
;; be caught.
;;
;; Faces are global to the Emacs process and cannot be un-defined, so this is
;; a single lifecycle test rather than two, the same reasoning as
;; `apply-diff-hl-faces/tracks-diff-hl-load-lifecycle': it exercises the "not
;; yet defined" no-op path (real absence -- batch `emacs -Q' never defines
;; Gnus's faces) before creating the face, then the "defined" path.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest gnus-group-news-low-fix/tracks-face-existence ()
  "No-op before `gnus-group-news-low-empty' exists; repointed once it does,
and the repointing survives a fresh frame's face recalculation -- unlike a
plain `set-face-attribute', which only patches the face's current value and
gets silently overwritten the next time some frame (Corfu's popup, via
`x-create-frame-with-faces') recalculates it from the enabled theme's own
registered spec table."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-gnus-group-news-low-fix)
  (deftheme cfg-test-fake-theme)
  (unwind-protect
      (progn
        ;; Act / Assert -- startup: the face does not exist yet.
        (should-not (facep 'gnus-group-news-low-empty))
        (should (progn (my/apply-gnus-group-news-low-fix 'cfg-test-fake-theme) t))
        ;; Arrange -- Doom Themes (or Gnus) defines the face, cyclically, as
        ;; part of the fake theme's own registered spec (matching how the
        ;; real bug is registered under `doom-dracula'/`doom-solarized-light').
        (make-face 'gnus-group-news-low-empty)
        (make-face 'gnus-group-news-low)
        (make-face 'gnus-group-mail-1-empty)
        (custom-theme-set-faces
         'cfg-test-fake-theme
         '(gnus-group-news-low-empty ((t (:inherit gnus-group-news-low)))))
        (enable-theme 'cfg-test-fake-theme)
        ;; Act
        (my/apply-gnus-group-news-low-fix 'cfg-test-fake-theme)
        ;; Assert -- repointed away from the cyclic inherit.
        (should (eq (face-attribute 'gnus-group-news-low-empty :inherit)
                    'gnus-group-mail-1-empty))
        ;; Act -- simulate what a brand new frame's face recalculation does.
        (face-spec-recalc 'gnus-group-news-low-empty (selected-frame))
        ;; Assert -- still repointed, not reverted to the cyclic inherit; a
        ;; plain `set-face-attribute' fix would not survive this step.
        (should (eq (face-attribute 'gnus-group-news-low-empty :inherit)
                    'gnus-group-mail-1-empty)))
    (disable-theme 'cfg-test-fake-theme)))

(provide 'gnus-group-news-low-fix-test)
;;; gnus-group-news-low-fix-test.el ends here
