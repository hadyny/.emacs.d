;;; modus-variant-test.el --- Tests for the Modus Themes variant switch -*- lexical-binding: t; -*-

;; The configuration follows the system appearance with Modus Themes:
;; `modus-vivendi-tinted' (the dark variant) and `modus-operandi-tinted' (the
;; light one).
;;
;; Unlike Catppuccin -- one theme with a `catppuccin-flavor' variable -- Modus
;; Themes ships each variant as its own theme.  So the switch is a
;; `modus-themes-load-theme' call, which already disables every other Modus
;; variant before loading the new one: plain `load-theme' stacks rather than
;; replaces, and a lingering variant would leave faces neither theme
;; specifies showing through with the wrong colours.
;;
;; `my/modus-variant-for' is the pure appearance -> theme-symbol map the whole
;; switch is built on; `my/apply-modus-variant' is the effectful part that
;; loads it and repairs the faces `load-theme' re-specs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest modus-variant/light-is-operandi ()
  "A light appearance selects the Operandi Tinted variant."
  ;; Arrange
  (cfg-test-load-defun 'my/modus-variant-for)
  ;; Act
  (let ((variant (my/modus-variant-for 'light)))
    ;; Assert
    (should (eq variant 'modus-operandi-tinted))))

(ert-deftest modus-variant/dark-is-vivendi ()
  "A dark appearance selects the Vivendi Tinted variant."
  ;; Arrange
  (cfg-test-load-defun 'my/modus-variant-for)
  ;; Act
  (let ((variant (my/modus-variant-for 'dark)))
    ;; Assert
    (should (eq variant 'modus-vivendi-tinted))))

(ert-deftest modus-variant/unknown-defaults-to-vivendi ()
  "Anything other than `light' falls back to the dark Vivendi Tinted variant.
The no-detection path in the auto-dark block relies on this."
  ;; Arrange
  (cfg-test-load-defun 'my/modus-variant-for)
  ;; Act
  (let ((variant (my/modus-variant-for nil)))
    ;; Assert
    (should (eq variant 'modus-vivendi-tinted))))

(ert-deftest modus-variant/apply-loads-the-right-variant ()
  "The switch delegates to `modus-themes-load-theme' with the right variant.
That function already disables every other Modus variant before loading the
new one, so there is no manual `disable-theme' call to make here -- unlike
the Tokyo Night switch this replaced."
  ;; Arrange
  (cfg-test-load-defun 'my/modus-variant-for)
  (cfg-test-load-defun 'my/apply-modus-variant)
  (let (calls)
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme &rest _) (push theme calls)))
              ((symbol-function 'my/apply-diff-hl-faces) #'ignore)
              ((symbol-function 'my/apply-font-faces) #'ignore))
      ;; Act
      (my/apply-modus-variant 'light)
      ;; Assert
      (should (equal calls '(modus-operandi-tinted))))))

(ert-deftest modus-variant/apply-repairs-the-themed-faces ()
  "Loading a variant re-applies the diff-hl colours and the font attributes.
`load-theme' re-specs `default' and the diff-hl faces, so both helpers must run
on every switch -- the same contract the Catppuccin flavour hook had."
  ;; Arrange
  (cfg-test-load-defun 'my/modus-variant-for)
  (cfg-test-load-defun 'my/apply-modus-variant)
  (let (calls)
    (cl-letf (((symbol-function 'modus-themes-load-theme) #'ignore)
              ((symbol-function 'my/apply-diff-hl-faces)
               (lambda () (push 'diff-hl calls)))
              ((symbol-function 'my/apply-font-faces)
               (lambda () (push 'fonts calls))))
      ;; Act
      (my/apply-modus-variant 'dark)
      ;; Assert
      (should (memq 'diff-hl calls))
      (should (memq 'fonts calls)))))

(ert-deftest modus-variant/package-is-in-the-closure ()
  "flake.nix ships `modus-themes' and no longer ships `tokyo-night'.
Both variants come from the one package (protesilaos/modus-themes), which also
provides the `modus-themes-get-color-value' palette lookup the diff-hl colours
use.  It is taken from GNU ELPA rather than the copy bundled with Emacs, which
lags a full major version behind."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList")))
    ;; Assert
    (should (member "modus-themes" packages))
    (should-not (member "tokyo-night" packages))
    (should-not (member "catppuccin-theme" packages))))

(ert-deftest modus-variant/no-tokyo-night-symbols-remain ()
  "No `tokyo-night-' symbol survives in the tangled config.
The Tokyo Night *strings* in the Themes prose comparing it to Modus Themes are
just commentary and do not reach config.el; this checks the code, which would
otherwise call now-void functions the moment the package leaves the closure."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms)))
        offenders)
    (dolist (symbol '("tokyo-night-get-color" "tokyo-night-flat-mode-line"
                      "my/tokyo-night-variant-for" "my/apply-tokyo-night-variant"))
      (when (string-match-p symbol code) (push symbol offenders)))
    ;; Assert
    (should (null offenders))))

(ert-deftest modus-variant/no-other-theme-is-loaded ()
  "Every `modus-themes-load-theme' call literally names a Modus variant, and
no plain `load-theme' call names a theme of its own.
`doom-themes' stays for its visual-bell/treemacs/org configs, but nothing may
load a theme of its own: a second theme loaded via plain `load-theme' would
show through wherever Modus Themes leaves a face unspecified.  Calls that pass
a variable (the appearance switch itself) are covered by the tests above."
  ;; Arrange / Act
  (let ((variants '(modus-vivendi-tinted modus-operandi-tinted))
        offenders)
    (dolist (form (cfg-test-read-forms))
      (dolist (call (append (cfg-test-find-all form 'load-theme)
                            (cfg-test-find-all form 'modus-themes-load-theme)))
        (let ((arg (nth 1 call)))
          ;; Only literal `(...-load-theme 'foo ...)' calls can be judged here.
          (when (and (eq (car-safe arg) 'quote)
                     (not (memq (cadr arg) variants)))
            (push (cadr arg) offenders)))))
    ;; Assert
    (should (null offenders))))

(provide 'modus-variant-test)
;;; modus-variant-test.el ends here
