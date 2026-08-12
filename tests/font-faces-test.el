;;; font-faces-test.el --- Tests for my/apply-font-faces -*- lexical-binding: t; -*-

;; Code is Maple Mono NF: `default', `fixed-pitch', the fallback glyph face and
;; the nerd-icons family.  Prose (`variable-pitch') is Inter, chosen on x-height
;; -- 0.546em against Maple Mono's 0.550em -- so both look the same size at one
;; `:height' and `mixed-pitch' needs no fudge factor.
;;
;; With one family for code, no face needs its own: an unspecified family falls
;; through to `default' at render time.  Only two things fail to survive
;; `catppuccin-reload' and must be re-applied from the flavour hook, the same
;; reason `my/apply-diff-hl-faces' exists:
;;
;;   * `default' is themed by catppuccin, so a reload strips family/height/weight;
;;   * `italic', comment and keyword lose their *slant*, so a light/dark switch
;;     (or auto-dark's startup re-apply) would drop italics everywhere.
;;
;; Ligatures need no package: Maple Mono's `calt' holds chaining lookups that
;; swap each character for a partial glyph, so `->' reads as an arrow across two
;; cells with nothing composed.  Adding `ligature' would compose the pair and
;; fight the font, so its absence is asserted.

;;; Code:

(require 'ert)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(ert-deftest font-faces/reapplies-base-default-font ()
  "The helper re-applies the base default font size and weight.
`catppuccin-reload' re-specs the (themed) `default' face and strips these, so
they must be re-applied from the flavour hook, not just once at top level."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-font-faces)
  ;; Simulate a post-reload `default' face with height/weight stripped.
  (set-face-attribute 'default nil :height 100 :weight 'normal)
  ;; Act
  (my/apply-font-faces)
  ;; Assert
  (should (eq (face-attribute 'default :height) 140))
  (should (eq (face-attribute 'default :weight) 'medium)))

(ert-deftest font-faces/reapplies-italic-slant ()
  "The italic faces keep their slant across a flavour reload.
They are theme-restyled, so setting the slant once at top level is not enough:
`catppuccin-reload' drops it and italics vanish everywhere.  Family is
deliberately *not* set -- it falls through to `default'."
  ;; Arrange
  (cfg-test-load-defun 'my/apply-font-faces)
  (dolist (face '(italic font-lock-comment-face font-lock-keyword-face))
    (set-face-attribute face nil :slant 'normal))
  ;; Act
  (my/apply-font-faces)
  ;; Assert
  (dolist (face '(italic font-lock-comment-face font-lock-keyword-face))
    (should (eq (face-attribute face :slant) 'italic))))

(ert-deftest font-faces/code-is-maple-mono-prose-is-inter ()
  "Code faces are Maple Mono NF; `variable-pitch' is Inter."
  ;; Arrange / Act
  (let ((code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should (string-match-p "set-face-attribute 'default nil :font \"Maple Mono NF\"" code))
    (should (string-match-p "nerd-icons-font-family \"Maple Mono NF\"" code))
    (should (string-match-p "'variable-pitch nil[[:space:]]+:font \"Inter\"" code))
    ;; `fixed-pitch' -- code inside prose buffers -- must stay with the code font.
    (should (string-match-p "'fixed-pitch nil[[:space:]]+:font \"Maple Mono NF\"" code))
    (should-not (string-match-p "Monaspice" code))))

(ert-deftest font-faces/no-face-carries-a-redundant-family ()
  "`my/apply-font-faces' sets a family only on `default'.
With one typeface everywhere, a per-face `:family' is dead weight that has to be
found and changed again next time the font moves."
  ;; Arrange / Act
  (let* ((defun-form (cfg-test-find-defun (cons 'progn (cfg-test-read-forms))
                                          'my/apply-font-faces))
         (printed (prin1-to-string defun-form)))
    ;; Assert -- `default' gets `:font' (family plus size and weight in one);
    ;; nothing else should name a family at all.
    (should defun-form)
    (should (string-match-p ":font \"Maple Mono NF\"" printed))
    (should-not (string-match-p ":family" printed))))

(ert-deftest font-faces/ligature-package-is-not-wired ()
  "`ligature' stays out: Maple Mono draws its arrows without composition."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (member "ligature" packages))
    (should-not (string-match-p "ligature" code))))

(ert-deftest font-faces/no-top-level-face-reads ()
  "No top-level form reads a realised face value.
`(face-attribute ...)' at load time is answered before the theme has been
applied -- and in a daemon before any frame exists at all -- so anything derived
from it is pinned to the wrong colour until the next flavour switch re-specs the
face.  Faces the theme owns must be left to the theme; anything that genuinely
needs to read one belongs in a function called from the flavour hook, like
`my/apply-diff-hl-faces'.

This caught `(set-face-background \='fringe (face-attribute \='default
:background))', which produced a mis-coloured fringe in the first client frame
of an Emacs daemon.  It was redundant too: catppuccin already specifies
`(fringe :background ctp-base)', the same base as `default'."
  ;; Arrange / Act
  (let (offenders)
    (dolist (form (cfg-test-read-forms))
      (unless (memq (car-safe form) '(defun use-package with-eval-after-load))
        (let ((printed (prin1-to-string form)))
          ;; "(face-attribute" does not match "(set-face-attribute" -- the
          ;; character before `face-' is `-', not `('.
          (when (string-match-p "(\\(face-attribute\\|face-background\\|face-foreground\\)"
                                printed)
            (push (substring printed 0 (min 70 (length printed))) offenders)))))
    ;; Assert
    (should (null offenders))))

(provide 'font-faces-test)
;;; font-faces-test.el ends here
