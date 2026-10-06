;;; theme-variant-test.el --- Tests for the Catppuccin flavour switch -*- lexical-binding: t; -*-

;; The configuration follows the system appearance with Catppuccin: Mocha (the
;; dark flavour) and Latte (the light one).
;;
;; Catppuccin ships one theme, `catppuccin', and reads `catppuccin-flavor' when
;; the theme file loads.  So the switch sets the flavour first, then runs
;; `load-theme', preceded by `disable-theme' on every currently enabled theme:
;; `load-theme' stacks rather than replaces, and a lingering theme would leave
;; faces Catppuccin does not specify showing through with the wrong colours.
;;
;; `my/theme-for-appearance' is the pure appearance -> flavour map the whole
;; switch is built on; `my/apply-theme-for-appearance' is the effectful part
;; that loads it and repairs the faces `load-theme' re-specs, including
;; flattening the mode-line back to `default''s background with a top rule
;; (`my/apply-modeline-face').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defvar catppuccin-flavor)

(defmacro theme-variant-test--with-stubbed-helpers (bindings &rest body)
  "Run BODY with every face-repair helper stubbed to `ignore'.
BINDINGS are extra `cl-letf' bindings that take precedence over the stubs."
  (declare (indent 1))
  `(cl-letf* (((symbol-function 'my/apply-diff-hl-faces) #'ignore)
              ((symbol-function 'my/apply-fringe-face) #'ignore)
              ((symbol-function 'my/apply-font-faces) #'ignore)
              ((symbol-function 'my/apply-modeline-face) #'ignore)
              ((symbol-function 'my/apply-centaur-tabs-faces) #'ignore)
              ,@bindings)
     ,@body))

(ert-deftest theme-variant/light-is-latte ()
  "A light appearance selects the Latte flavour."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  ;; Act
  (let ((flavor (my/theme-for-appearance 'light)))
    ;; Assert
    (should (eq flavor 'latte))))

(ert-deftest theme-variant/dark-is-mocha ()
  "A dark appearance selects the Mocha flavour."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  ;; Act
  (let ((flavor (my/theme-for-appearance 'dark)))
    ;; Assert
    (should (eq flavor 'mocha))))

(ert-deftest theme-variant/unknown-defaults-to-mocha ()
  "Anything other than `light' falls back to the dark Mocha flavour.
The no-detection path in the auto-dark block relies on this."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  ;; Act
  (let ((flavor (my/theme-for-appearance nil)))
    ;; Assert
    (should (eq flavor 'mocha))))

(ert-deftest theme-variant/apply-disables-enabled-themes-and-loads-catppuccin ()
  "The switch disables every enabled theme, then loads `catppuccin'."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  (cfg-test-load-defun 'my/apply-theme-for-appearance)
  (let ((catppuccin-flavor 'latte)
        disabled loaded)
    (theme-variant-test--with-stubbed-helpers
        (((symbol-function 'disable-theme)
          (lambda (theme) (push theme disabled)))
         ((symbol-function 'load-theme)
          (lambda (theme &rest _) (push theme loaded)))
         ((symbol-value 'custom-enabled-themes) '(catppuccin)))
      ;; Act
      (my/apply-theme-for-appearance 'dark)
      ;; Assert
      (should (equal disabled '(catppuccin)))
      (should (equal loaded '(catppuccin))))))

(ert-deftest theme-variant/apply-sets-flavor-before-loading ()
  "`catppuccin-flavor' is already the new flavour when `load-theme' runs.
The theme file reads the flavour as it loads, so setting it afterwards would
leave the previous flavour's colours on screen until the next switch."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  (cfg-test-load-defun 'my/apply-theme-for-appearance)
  (let ((catppuccin-flavor 'mocha)
        flavor-at-load)
    (theme-variant-test--with-stubbed-helpers
        (((symbol-function 'disable-theme) #'ignore)
         ((symbol-function 'load-theme)
          (lambda (&rest _) (setq flavor-at-load catppuccin-flavor)))
         ((symbol-value 'custom-enabled-themes) nil))
      ;; Act
      (my/apply-theme-for-appearance 'light)
      ;; Assert
      (should (eq flavor-at-load 'latte)))))

(ert-deftest theme-variant/apply-repairs-the-themed-faces ()
  "Loading a flavour re-applies the diff-hl, fringe, font, mode-line and
centaur-tabs faces.  `load-theme' re-specs `default', the diff-hl faces, the
fringe, the mode-line and the selected tab, so every helper must run on each
switch."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  (cfg-test-load-defun 'my/apply-theme-for-appearance)
  (let ((catppuccin-flavor 'latte)
        calls)
    (theme-variant-test--with-stubbed-helpers
        (((symbol-function 'disable-theme) #'ignore)
         ((symbol-function 'load-theme) #'ignore)
         ((symbol-value 'custom-enabled-themes) nil)
         ((symbol-function 'my/apply-diff-hl-faces)
          (lambda () (push 'diff-hl calls)))
         ((symbol-function 'my/apply-fringe-face)
          (lambda () (push 'fringe calls)))
         ((symbol-function 'my/apply-font-faces)
          (lambda () (push 'fonts calls)))
         ((symbol-function 'my/apply-modeline-face)
          (lambda () (push 'modeline calls)))
         ((symbol-function 'my/apply-centaur-tabs-faces)
          (lambda () (push 'centaur-tabs calls))))
      ;; Act
      (my/apply-theme-for-appearance 'dark)
      ;; Assert
      (dolist (helper '(diff-hl fringe fonts modeline centaur-tabs))
        (should (memq helper calls))))))

(ert-deftest theme-variant/modeline-face-matches-default-background ()
  "The mode-line faces get `default''s background, no box, and a mauve overline.
The `bar' faces blend into that background instead of losing the segment
outright, and the cached bar images are refreshed so the colour actually
changes visibly."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-modeline-face)
  (let (calls refreshed)
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (face attr &rest _)
                 (cond ((and (eq face 'default) (eq attr :background)) "#123456")
                       (t nil))))
              ((symbol-function 'catppuccin-color)
               (lambda (key &optional _flavor)
                 (should (symbolp key))
                 (when (eq key 'mauve) "#cba6f7")))
              ((symbol-function 'facep) (lambda (_face) t))
              ((symbol-function 'doom-modeline-refresh-bars)
               (lambda () (setq refreshed t)))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest plist) (push (cons face plist) calls))))
      ;; Act
      (my/apply-modeline-face)
      ;; Assert
      (dolist (face '(mode-line mode-line-active mode-line-inactive))
        (let ((plist (cdr (assq face calls))))
          (should (equal (plist-get plist :background) "#123456"))
          (should (null (plist-get plist :box)))
          (should (equal (plist-get plist :overline) "#cba6f7"))))
      (dolist (face '(doom-modeline-bar doom-modeline-bar-inactive))
        (let ((plist (cdr (assq face calls))))
          (should (equal (plist-get plist :background) "#123456"))))
      (should refreshed))))

(ert-deftest theme-variant/package-is-in-the-closure ()
  "flake.nix ships `catppuccin-theme' and no other theme package.
Both flavours come from the one package, which also provides the
`catppuccin-color' palette lookup the face helpers use."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList")))
    ;; Assert
    (should (member "catppuccin-theme" packages))
    (dolist (stale '("doom-themes" "modus-themes" "tokyo-night"))
      (should-not (member stale packages)))))

(ert-deftest theme-variant/no-stale-theme-symbols-remain ()
  "No symbol from a previous theme package survives in the tangled config.
Prose that mentions them does not reach config.el; this checks the code,
which would otherwise call now-void functions the moment the package leaves
the closure."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms)))
        offenders)
    (dolist (symbol '("doom-color" "doom-themes-" "doom-dracula"
                      "doom-solarized-light" "my/apply-gnus-group-news-low-fix"
                      "tokyo-night-get-color" "tokyo-night-flat-mode-line"
                      "my/tokyo-night-variant-for" "my/apply-tokyo-night-variant"
                      "modus-themes-load-theme" "modus-themes-get-color-value"
                      "my/modus-variant-for" "my/apply-modus-variant"))
      (when (string-match-p (regexp-quote symbol) code) (push symbol offenders)))
    ;; Assert
    (should (null offenders))))

(ert-deftest theme-variant/no-other-theme-is-loaded ()
  "Every literal `load-theme' call names `catppuccin'.
A second theme loaded via plain `load-theme' would show through wherever
Catppuccin leaves a face unspecified."
  ;; Arrange / Act
  (let (offenders)
    (dolist (form (cfg-test-read-forms))
      (dolist (call (cfg-test-find-all form 'load-theme))
        (let ((arg (nth 1 call)))
          ;; Only literal `(load-theme 'foo ...)' calls can be judged here.
          (when (and (eq (car-safe arg) 'quote)
                     (not (eq (cadr arg) 'catppuccin)))
            (push (cadr arg) offenders)))))
    ;; Assert
    (should (null offenders))))

(ert-deftest theme-variant/auto-dark-loads-after-catppuccin ()
  "`use-package auto-dark' waits for `catppuccin-theme'.
Its `:config' calls `my/apply-theme-for-appearance', which is defined in the
`catppuccin-theme' block and calls `catppuccin-color'."
  ;; Arrange / Act
  (let (form)
    (dolist (f (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all f 'use-package))
        (when (eq (nth 1 up) 'auto-dark) (setq form up))))
    ;; Assert
    (should form)
    (should (eq (cadr (memq :after form)) 'catppuccin-theme))))

(provide 'theme-variant-test)
;;; theme-variant-test.el ends here
