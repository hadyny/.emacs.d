;;; tab-bar-faces-test.el --- Tests for my/apply-tab-bar-faces -*- lexical-binding: t; -*-

;; `tab-bar-mode' tabs are styled to match the centaur-tabs row: a flat
;; `default' background with the selected tab marked by a `mauve' underline.
;; Catppuccin's own spec gives `tab-bar-tab' a `ctp-current' fill and
;; `load-theme' resets every face it touches, so `my/apply-tab-bar-faces' must
;; re-run on every variant switch, the same way `my/apply-centaur-tabs-faces'
;; does (covered by `theme-variant/apply-repairs-the-themed-faces').
;;
;; Inactive tabs use `subtext1': `subtext0' is only 4.37:1 against Latte's
;; `base', below the WCAG 2.2 AA 4.5:1 minimum for text.
;;
;; tab-bar.el is preloaded, so its faces exist even in batch `emacs -Q' and the
;; function needs no `facep' guard.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defconst tab-bar-faces-test--palette
  '((mauve . "#cba6f7") (text . "#cdd6f4") (subtext1 . "#bac2de")
    (surface0 . "#313244"))
  "Mocha values returned by the stubbed `catppuccin-color'.")

(defmacro tab-bar-faces-test--with-theme (bg &rest body)
  "Run BODY with `default''s background BG and `catppuccin-color' stubbed.
Each face under test is reset first so attributes from an earlier test do
not leak into the next."
  (declare (indent 1))
  `(let ((orig-face-attribute (symbol-function 'face-attribute)))
     (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive
                     tab-bar-tab-highlight))
       (face-spec-reset-face face))
     (cl-letf (((symbol-function 'catppuccin-color)
                (lambda (name) (alist-get name tab-bar-faces-test--palette)))
               ((symbol-function 'face-attribute)
                (lambda (face attr &optional frame inherit)
                  (if (and (eq face 'default) (eq attr :background))
                      ,bg
                    (funcall orig-face-attribute face attr frame inherit)))))
       ,@body)))

(ert-deftest apply-tab-bar-faces/bar-background-matches-default ()
  "The whole bar is flattened to `default''s background."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    ;; Act
    (my/apply-tab-bar-faces)
    ;; Assert
    (should (equal (face-attribute 'tab-bar :background) "#1e1e2e"))
    (should (equal (face-attribute 'tab-bar-tab :background) "#1e1e2e"))
    (should (equal (face-attribute 'tab-bar-tab-inactive :background) "#1e1e2e"))))

(ert-deftest apply-tab-bar-faces/selected-tab-has-mauve-underline ()
  "The selected tab is marked by a `mauve' underline in `text' colour."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    ;; Act
    (my/apply-tab-bar-faces)
    ;; Assert
    (should (equal (face-attribute 'tab-bar-tab :underline) "#cba6f7"))
    (should (equal (face-attribute 'tab-bar-tab :foreground) "#cdd6f4"))))

(ert-deftest apply-tab-bar-faces/inactive-tab-has-no-underline ()
  "Inactive tabs carry no underline and use the dimmer `subtext1'."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    ;; Act
    (my/apply-tab-bar-faces)
    ;; Assert
    (should-not (face-attribute 'tab-bar-tab-inactive :underline))
    (should (equal (face-attribute 'tab-bar-tab-inactive :foreground) "#bac2de"))))

(ert-deftest apply-tab-bar-faces/hover-fills-tab-with-surface0 ()
  "Hovering a tab fills it with `surface0' in `text' colour.
`tab-bar-tab-highlight' is the `mouse-face' that
`tab-bar-tab-name-format-mouse-face' puts on every tab name.  Its built-in
spec is a grey85/black raised button, which Catppuccin does not override."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    ;; Act
    (my/apply-tab-bar-faces)
    ;; Assert
    (should (equal (face-attribute 'tab-bar-tab-highlight :background) "#313244"))
    (should (equal (face-attribute 'tab-bar-tab-highlight :foreground) "#cdd6f4"))))

(ert-deftest apply-tab-bar-faces/hover-keeps-tab-size ()
  "The hover box has the same padding as the tabs, coloured `surface0'.
A different `:line-width' would make the tab grow or shrink under the
pointer, and the built-in `released-button' style draws a 3D border."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    ;; Act
    (my/apply-tab-bar-faces)
    ;; Assert
    (let ((hover (face-attribute 'tab-bar-tab-highlight :box))
          (tab (face-attribute 'tab-bar-tab :box)))
      (should (equal (plist-get hover :line-width) (plist-get tab :line-width)))
      (should (equal (plist-get hover :color) "#313244"))
      (should-not (plist-get hover :style)))))

(ert-deftest apply-tab-bar-faces/hover-leaves-underline-to-the-tab ()
  "The hover face does not set `:underline'.
`mouse-face' merges over the tab's own face, so leaving it unspecified keeps
the `mauve' underline on the selected tab and adds none to inactive tabs."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    ;; Act
    (my/apply-tab-bar-faces)
    ;; Assert
    (should (eq (face-attribute 'tab-bar-tab-highlight :underline) 'unspecified))))

(ert-deftest apply-tab-bar-faces/tracks-flavour-switch ()
  "A second call with a new `default' background replaces the first."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-tab-bar-faces)
  (tab-bar-faces-test--with-theme "#1e1e2e"
    (my/apply-tab-bar-faces))
  ;; Act
  (cl-letf (((symbol-function 'catppuccin-color)
             (lambda (name) (alist-get name tab-bar-faces-test--palette)))
            ((symbol-function 'face-attribute)
             (let ((orig (symbol-function 'face-attribute)))
               (lambda (face attr &optional frame inherit)
                 (if (and (eq face 'default) (eq attr :background))
                     "#eff1f5"
                   (funcall orig face attr frame inherit))))))
    (my/apply-tab-bar-faces))
  ;; Assert
  (should (equal (face-attribute 'tab-bar :background) "#eff1f5"))
  (should (equal (face-attribute 'tab-bar-tab :background) "#eff1f5")))

(ert-deftest tab-bar/mode-enabled-in-core-defaults ()
  "`tab-bar-mode' is switched on, so the styled tabs are actually shown.
Commit 491bc99 dropped the call when centaur-tabs arrived, which left the
workspace bindings `] t'/`[ t' cycling tabs that were never drawn."
  ;; Arrange
  (let ((forms (cfg-test-read-forms)))
    ;; Act
    (let ((calls (cl-mapcan (lambda (f) (cfg-test-find-all f 'tab-bar-mode))
                            forms)))
      ;; Assert
      (should (member '(tab-bar-mode 1) calls)))))

;;; tab-bar-faces-test.el ends here
