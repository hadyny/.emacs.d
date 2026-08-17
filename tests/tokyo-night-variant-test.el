;;; tokyo-night-variant-test.el --- Tests for the Tokyo Night theme switch -*- lexical-binding: t; -*-

;; The configuration follows the system appearance with Tokyo Night: `tokyo-night'
;; (the dark "night" variant) and `tokyo-night-day' (the light one).
;;
;; Unlike Catppuccin -- one theme with a `catppuccin-flavor' variable -- Tokyo
;; Night ships each variant as its own theme.  So the switch is a `load-theme',
;; and the *previous* variant has to be disabled first: `load-theme' stacks
;; rather than replaces, and a day theme layered over a night one leaves faces
;; only the lower theme specifies showing through with dark colours.
;;
;; `my/tokyo-night-variant-for' is the pure appearance -> theme-symbol map the
;; whole switch is built on; `my/apply-tokyo-night-variant' is the effectful part
;; that loads it and repairs the faces `load-theme' re-specs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest tokyo-night-variant/light-is-day ()
  "A light appearance selects the Day variant."
  ;; Arrange
  (cfg-test-load-defun 'my/tokyo-night-variant-for)
  ;; Act
  (let ((variant (my/tokyo-night-variant-for 'light)))
    ;; Assert
    (should (eq variant 'tokyo-night-day))))

(ert-deftest tokyo-night-variant/dark-is-night ()
  "A dark appearance selects the Night variant."
  ;; Arrange
  (cfg-test-load-defun 'my/tokyo-night-variant-for)
  ;; Act
  (let ((variant (my/tokyo-night-variant-for 'dark)))
    ;; Assert
    (should (eq variant 'tokyo-night))))

(ert-deftest tokyo-night-variant/unknown-defaults-to-night ()
  "Anything other than `light' falls back to the dark Night variant.
The no-detection path in the auto-dark block relies on this."
  ;; Arrange
  (cfg-test-load-defun 'my/tokyo-night-variant-for)
  ;; Act
  (let ((variant (my/tokyo-night-variant-for nil)))
    ;; Assert
    (should (eq variant 'tokyo-night))))

(ert-deftest tokyo-night-variant/apply-disables-the-other-variant-first ()
  "The switch disables the outgoing variant before loading the incoming one.
Two Tokyo Night themes enabled at once is the failure mode this guards: faces the
day theme does not specify would keep their night colours."
  ;; Arrange
  (cfg-test-load-defun 'my/tokyo-night-variant-for)
  (cfg-test-load-defun 'my/apply-tokyo-night-variant)
  (let (calls)
    (cl-letf (((symbol-function 'load-theme)
               (lambda (theme &rest _) (push (list 'load theme) calls)))
              ((symbol-function 'disable-theme)
               (lambda (theme) (push (list 'disable theme) calls)))
              ((symbol-function 'my/apply-diff-hl-faces)
               (lambda () (push '(diff-hl) calls)))
              ((symbol-function 'my/apply-font-faces)
               (lambda () (push '(fonts) calls))))
      ;; Act
      (my/apply-tokyo-night-variant 'light)
      ;; Assert
      (let ((ordered (nreverse calls)))
        (should (member '(disable tokyo-night) ordered))
        (should (member '(load tokyo-night-day) ordered))
        (should (< (cl-position '(disable tokyo-night) ordered :test #'equal)
                   (cl-position '(load tokyo-night-day) ordered :test #'equal)))))))

(ert-deftest tokyo-night-variant/apply-repairs-the-themed-faces ()
  "Loading a variant re-applies the diff-hl colours and the font attributes.
`load-theme' re-specs `default' and the diff-hl faces, so both helpers must run
on every switch -- the same contract the Catppuccin flavour hook had."
  ;; Arrange
  (cfg-test-load-defun 'my/tokyo-night-variant-for)
  (cfg-test-load-defun 'my/apply-tokyo-night-variant)
  (let (calls)
    (cl-letf (((symbol-function 'load-theme) #'ignore)
              ((symbol-function 'disable-theme) #'ignore)
              ((symbol-function 'my/apply-diff-hl-faces)
               (lambda () (push 'diff-hl calls)))
              ((symbol-function 'my/apply-font-faces)
               (lambda () (push 'fonts calls))))
      ;; Act
      (my/apply-tokyo-night-variant 'dark)
      ;; Assert
      (should (memq 'diff-hl calls))
      (should (memq 'fonts calls)))))

(ert-deftest tokyo-night-variant/package-is-in-the-closure ()
  "flake.nix ships `tokyo-night' and no longer ships `catppuccin-theme'.
Both variants come from the one package (bbatsov/tokyo-night-emacs), which also
provides the `tokyo-night-get-color' palette lookup the diff-hl colours use."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList")))
    ;; Assert
    (should (member "tokyo-night" packages))
    (should-not (member "catppuccin-theme" packages))))

(ert-deftest tokyo-night-variant/no-catppuccin-symbols-remain ()
  "No `catppuccin-' symbol survives in the tangled config.
The Catppuccin *strings* in the magit-delta block are deliberate and stay -- delta
0.19.2 has no Tokyo Night syntax theme (see magit-delta-test.el).  This checks the
code, not those strings: `catppuccin-flavor', `catppuccin-reload' and
`catppuccin-color' are all void once the package leaves the closure."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms)))
        offenders)
    (dolist (symbol '("catppuccin-flavor" "catppuccin-reload" "catppuccin-color"
                      "catppuccin-theme"))
      (when (string-match-p symbol code) (push symbol offenders)))
    ;; Assert
    (should (null offenders))))

(ert-deftest tokyo-night-variant/no-other-theme-is-loaded ()
  "Every `load-theme' that names a theme literally names a Tokyo Night variant.
`doom-themes' stays for its visual-bell/treemacs/org configs, but nothing may
load a theme of its own: `load-theme' stacks, so a second theme would show
through wherever Tokyo Night leaves a face unspecified.  Calls that pass a
variable (the appearance switch itself) are covered by the tests above."
  ;; Arrange / Act
  (let ((variants '(tokyo-night tokyo-night-day tokyo-night-storm tokyo-night-moon))
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

(provide 'tokyo-night-variant-test)
;;; tokyo-night-variant-test.el ends here
