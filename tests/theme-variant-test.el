;;; theme-variant-test.el --- Tests for the Doom Themes variant switch -*- lexical-binding: t; -*-

;; The configuration follows the system appearance with Doom Themes:
;; `doom-dracula' (the dark variant) and `doom-solarized-light' (the light
;; one).
;;
;; Doom Themes ships each variant as its own theme, so the switch is a plain
;; `load-theme' call, preceded by `disable-theme' on every currently enabled
;; theme: `load-theme' stacks rather than replaces, and a lingering variant
;; would leave faces neither theme specifies showing through with the wrong
;; colours.
;;
;; `my/theme-for-appearance' is the pure appearance -> theme-symbol map the
;; whole switch is built on; `my/apply-theme-for-appearance' is the effectful
;; part that loads it and repairs the faces `load-theme' re-specs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest theme-variant/light-is-solarized-light ()
  "A light appearance selects the Doom Solarized Light variant."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  ;; Act
  (let ((variant (my/theme-for-appearance 'light)))
    ;; Assert
    (should (eq variant 'doom-solarized-light))))

(ert-deftest theme-variant/dark-is-dracula ()
  "A dark appearance selects the Doom Dracula variant."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  ;; Act
  (let ((variant (my/theme-for-appearance 'dark)))
    ;; Assert
    (should (eq variant 'doom-dracula))))

(ert-deftest theme-variant/unknown-defaults-to-dracula ()
  "Anything other than `light' falls back to the dark Dracula variant.
The no-detection path in the auto-dark block relies on this."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  ;; Act
  (let ((variant (my/theme-for-appearance nil)))
    ;; Assert
    (should (eq variant 'doom-dracula))))

(ert-deftest theme-variant/apply-loads-the-right-variant ()
  "The switch disables every enabled theme, then loads the right variant."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  (cfg-test-load-defun 'my/apply-theme-for-appearance)
  (let (disabled loaded)
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled)))
              ((symbol-function 'load-theme)
               (lambda (theme &rest _) (push theme loaded)))
              ((symbol-value 'custom-enabled-themes) '(doom-solarized-light))
              ((symbol-function 'my/apply-diff-hl-faces) #'ignore)
              ((symbol-function 'my/apply-fringe-face) #'ignore)
              ((symbol-function 'my/apply-font-faces) #'ignore)
              ((symbol-function 'my/apply-gnus-group-news-low-fix) #'ignore))
      ;; Act
      (my/apply-theme-for-appearance 'dark)
      ;; Assert
      (should (equal disabled '(doom-solarized-light)))
      (should (equal loaded '(doom-dracula))))))

(ert-deftest theme-variant/apply-passes-loaded-variant-to-gnus-fix ()
  "The Gnus fix must be re-registered on the variant just loaded, not some
other theme symbol -- `custom-theme-set-faces' only survives future frame
recalculation if it targets the theme actually enabled (see
`my/apply-gnus-group-news-low-fix')."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  (cfg-test-load-defun 'my/apply-theme-for-appearance)
  (let (gnus-fix-theme)
    (cl-letf (((symbol-function 'disable-theme) #'ignore)
              ((symbol-function 'load-theme) #'ignore)
              ((symbol-value 'custom-enabled-themes) nil)
              ((symbol-function 'my/apply-diff-hl-faces) #'ignore)
              ((symbol-function 'my/apply-fringe-face) #'ignore)
              ((symbol-function 'my/apply-font-faces) #'ignore)
              ((symbol-function 'my/apply-gnus-group-news-low-fix)
               (lambda (theme) (setq gnus-fix-theme theme))))
      ;; Act
      (my/apply-theme-for-appearance 'light)
      ;; Assert
      (should (eq gnus-fix-theme 'doom-solarized-light)))))

(ert-deftest theme-variant/apply-repairs-the-themed-faces ()
  "Loading a variant re-applies the diff-hl colours, fonts and the Gnus fix.
`load-theme' re-specs `default', the diff-hl faces and Doom's buggy
`gnus-group-news-low-empty' inherit, so every helper must run on each switch
-- the same contract the Catppuccin flavour hook had."
  ;; Arrange
  (cfg-test-load-defun 'my/theme-for-appearance)
  (cfg-test-load-defun 'my/apply-theme-for-appearance)
  (let (calls)
    (cl-letf (((symbol-function 'disable-theme) #'ignore)
              ((symbol-function 'load-theme) #'ignore)
              ((symbol-value 'custom-enabled-themes) nil)
              ((symbol-function 'my/apply-diff-hl-faces)
               (lambda () (push 'diff-hl calls)))
              ((symbol-function 'my/apply-fringe-face)
               (lambda () (push 'fringe calls)))
              ((symbol-function 'my/apply-font-faces)
               (lambda () (push 'fonts calls)))
              ((symbol-function 'my/apply-gnus-group-news-low-fix)
               (lambda (_theme) (push 'gnus calls))))
      ;; Act
      (my/apply-theme-for-appearance 'dark)
      ;; Assert
      (should (memq 'diff-hl calls))
      (should (memq 'fringe calls))
      (should (memq 'fonts calls))
      (should (memq 'gnus calls)))))

(ert-deftest theme-variant/package-is-in-the-closure ()
  "flake.nix ships `doom-themes' and no longer ships `modus-themes' or
`tokyo-night'.  Both variants come from the one package (doomemacs/themes),
which also provides the `doom-color' palette lookup the diff-hl colours use."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList")))
    ;; Assert
    (should (member "doom-themes" packages))
    (should-not (member "modus-themes" packages))
    (should-not (member "tokyo-night" packages))
    (should-not (member "catppuccin-theme" packages))))

(ert-deftest theme-variant/no-tokyo-night-or-modus-symbols-remain ()
  "No `tokyo-night-' or `modus-themes-' symbol survives in the tangled config.
The Tokyo Night/Modus Themes *strings* in the Themes prose comparing them to
Doom Themes are just commentary and do not reach config.el; this checks the
code, which would otherwise call now-void functions the moment the package
leaves the closure."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms)))
        offenders)
    (dolist (symbol '("tokyo-night-get-color" "tokyo-night-flat-mode-line"
                      "my/tokyo-night-variant-for" "my/apply-tokyo-night-variant"
                      "modus-themes-load-theme" "modus-themes-get-color-value"
                      "my/modus-variant-for" "my/apply-modus-variant"))
      (when (string-match-p symbol code) (push symbol offenders)))
    ;; Assert
    (should (null offenders))))

(ert-deftest theme-variant/no-other-theme-is-loaded ()
  "Every `load-theme' call literally names a Doom Dracula/Solarized Light
variant, or a variable (the appearance switch itself).
`doom-themes' stays for its visual-bell/treemacs/org configs and now for the
variant themes themselves, but nothing may load a theme of its own: a second
theme loaded via plain `load-theme' would show through wherever the active
variant leaves a face unspecified."
  ;; Arrange / Act
  (let ((variants '(doom-dracula doom-solarized-light))
        offenders)
    (dolist (form (cfg-test-read-forms))
      (dolist (call (cfg-test-find-all form 'load-theme))
        (let ((arg (nth 1 call)))
          ;; Only literal `(load-theme 'foo ...)' calls can be judged here.
          (when (and (eq (car-safe arg) 'quote)
                     (not (memq (cadr arg) variants)))
            (push (cadr arg) offenders)))))
    ;; Assert
    (should (null offenders))))

(ert-deftest theme-variant/doom-themes-custom-options-are-set ()
  "`use-package doom-themes' opts into bold, italic, and a padded mode-line.
Bold/italic gate each variant's own per-face styling (see
`my/apply-font-faces', which deliberately leaves bold/italic alone and relies
on these instead); the padded mode-line is invisible to `svg-line' (see the
Mode-line section) but still worth enabling for any Emacs-drawn mode-line."
  ;; Arrange / Act
  (let (form)
    (dolist (f (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all f 'use-package))
        (when (eq (nth 1 up) 'doom-themes) (setq form up))))
    (let* ((tail (cdr (memq :custom form)))
           (custom-forms nil))
      (while (and tail (not (keywordp (car tail))))
        (push (car tail) custom-forms)
        (setq tail (cdr tail)))
      ;; Assert
      (should form)
      (should (member '(doom-themes-enable-bold t) custom-forms))
      (should (member '(doom-themes-enable-italic t) custom-forms))
      (should (member '(doom-themes-padded-modeline t) custom-forms)))))

(provide 'theme-variant-test)
;;; theme-variant-test.el ends here
