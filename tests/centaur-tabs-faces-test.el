;;; centaur-tabs-faces-test.el --- Tests for my/apply-centaur-tabs-faces -*- lexical-binding: t; -*-

;; `centaur-tabs-buffer-init' (hooked to `centaur-tabs-init-hook') only sets
;; the selected tab's `:underline'/`:overline' *once*, when `centaur-tabs-mode'
;; first enables. `load-theme' wipes it on every later switch: enabling a
;; theme resets every attribute of every face it touches to `unspecified'
;; before applying its own spec, and Doom Themes' own spec for
;; `centaur-tabs-selected' never mentions `:underline'/`:overline' at all.
;; `my/apply-centaur-tabs-faces' re-applies the colour on every variant
;; switch (see `my/apply-theme-for-appearance'), the same way
;; `my/apply-diff-hl-faces' and `my/apply-fringe-face' do for their faces.
;;
;; `centaur-tabs' is `:hook'ed rather than loaded eagerly, so the function
;; must be a no-op before it has loaded -- guarded by `featurep' the same way
;; `my/apply-diff-hl-faces' guards diff-hl's faces by `facep'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defvar centaur-tabs-set-bar nil
  "Stub for the real defcustom, absent since `centaur-tabs' is not loaded.")

(ert-deftest apply-centaur-tabs-faces/noop-before-centaur-tabs-loaded ()
  "No-op while `centaur-tabs' has not loaded (real absence in batch `emacs -Q')."
  (cfg-test-load-defun 'my/apply-centaur-tabs-faces)
  (cl-letf (((symbol-function 'featurep)
             (let ((orig (symbol-function 'featurep)))
               (lambda (feature &optional subfeature)
                 (if (eq feature 'centaur-tabs) nil (funcall orig feature subfeature))))))
    (should (progn (my/apply-centaur-tabs-faces) t))))

(ert-deftest apply-centaur-tabs-faces/under-sets-underline-not-overline ()
  "`centaur-tabs-set-bar' `under' re-applies the bar colour as an underline."
  (cfg-test-load-defun 'my/apply-centaur-tabs-faces)
  (unless (facep 'centaur-tabs-selected) (make-face 'centaur-tabs-selected))
  (unless (facep 'centaur-tabs-selected-modified) (make-face 'centaur-tabs-selected-modified))
  (unless (facep 'centaur-tabs-active-bar-face) (make-face 'centaur-tabs-active-bar-face))
  (let ((centaur-tabs-set-bar 'under))
    (cl-letf (((symbol-function 'featurep)
               (let ((orig (symbol-function 'featurep)))
                 (lambda (feature &optional subfeature)
                   (if (eq feature 'centaur-tabs) t (funcall orig feature subfeature)))))
              ((symbol-function 'face-background)
               (lambda (face &optional _frame _inherit)
                 (should (eq face 'centaur-tabs-active-bar-face))
                 "#bd93f9")))
      (my/apply-centaur-tabs-faces)
      (should (equal (face-attribute 'centaur-tabs-selected :underline) "#bd93f9"))
      (should-not (face-attribute 'centaur-tabs-selected :overline))
      (should (equal (face-attribute 'centaur-tabs-selected-modified :underline) "#bd93f9"))
      (should-not (face-attribute 'centaur-tabs-selected-modified :overline)))))

(ert-deftest apply-centaur-tabs-faces/over-sets-overline-not-underline ()
  "`centaur-tabs-set-bar' `over' re-applies the bar colour as an overline."
  (cfg-test-load-defun 'my/apply-centaur-tabs-faces)
  (unless (facep 'centaur-tabs-selected) (make-face 'centaur-tabs-selected))
  (unless (facep 'centaur-tabs-selected-modified) (make-face 'centaur-tabs-selected-modified))
  (unless (facep 'centaur-tabs-active-bar-face) (make-face 'centaur-tabs-active-bar-face))
  (let ((centaur-tabs-set-bar 'over))
    (cl-letf (((symbol-function 'featurep)
               (let ((orig (symbol-function 'featurep)))
                 (lambda (feature &optional subfeature)
                   (if (eq feature 'centaur-tabs) t (funcall orig feature subfeature)))))
              ((symbol-function 'face-background)
               (lambda (face &optional _frame _inherit)
                 (should (eq face 'centaur-tabs-active-bar-face))
                 "#bd93f9")))
      (my/apply-centaur-tabs-faces)
      (should (equal (face-attribute 'centaur-tabs-selected :overline) "#bd93f9"))
      (should-not (face-attribute 'centaur-tabs-selected :underline))
      (should (equal (face-attribute 'centaur-tabs-selected-modified :overline) "#bd93f9"))
      (should-not (face-attribute 'centaur-tabs-selected-modified :underline)))))

(provide 'centaur-tabs-faces-test)
;;; centaur-tabs-faces-test.el ends here
