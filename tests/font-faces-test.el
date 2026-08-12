;;; font-faces-test.el --- Tests for my/apply-font-faces -*- lexical-binding: t; -*-

;; Code is Maple Mono NF: `default', `fixed-pitch', the fallback glyph face and
;; the nerd-icons family.  Prose (`variable-pitch') is Inter, so `mixed-pitch' in
;; org and markdown actually changes typeface -- with Monaspace Xenon it never
;; did, because every Monaspace variant is monospaced.  Inter was picked on
;; x-height: 0.546em against Maple Mono's 0.550em, so both look the same size at
;; one `:height'.  Monaspace stays installed but unused.
;;
;; `my/apply-font-faces' therefore does much less than the role scheme it
;; replaced.  With one family everywhere, no face needs its own: an unspecified
;; family falls through to `default' at render time.  Only two things fail to
;; survive `catppuccin-reload' and must be re-applied from the flavour hook, the
;; same reason `my/apply-diff-hl-faces' exists:
;;
;;   * `default' is themed by catppuccin, so a reload strips family/height/weight;
;;   * `italic', comment and keyword lose their *slant*, so a light/dark switch
;;     (or auto-dark's startup re-apply) would drop italics everywhere.
;;
;; LIGATURES need no package with this font, which is why `ligature' is absent
;; and asserted so.  Maple Mono does not implement ligatures as ligatures at all:
;; there is no many-to-one GSUB substitution for `->'.  Its `calt' feature holds
;; *chaining* lookups covering `hyphen' that invoke *single* substitutions --
;; each character is swapped for a partial glyph, so the pair reads as an arrow
;; across two cells.  `calt' is on by default, so the font does this unaided.
;;
;; That is also why adding `ligature' back would be a mistake rather than a
;; no-op: it would compose the two characters into one unit and ask the font to
;; shape them together, which is not how these glyphs are built.  Monaspace was
;; the opposite case -- real ligatures, but behind stylistic sets Emacs cannot
;; enable on an NS build (ss01 Equal Symbols, ss03 Arrows, ...), and
;; `pyftfeatfreeze' cannot bake them in because it handles "only single and
;; alternate substitutions".

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
  "Code faces are Maple Mono NF; `variable-pitch' is Inter.
`variable-pitch' is the one face that deliberately differs, so `mixed-pitch'
has something to switch to.  Inter is chosen for its x-height: 0.546em against
Maple Mono's 0.550em, so prose and code look the same size at one `:height' and
no fudge factor is needed.  No face names Monaspace -- it stays installed, but
unused."
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
  "`ligature' stays out: Maple Mono needs no composition to draw its arrows.
See this file's header -- its `calt' swaps each character for a partial glyph, so
composing the pair would fight the font rather than help it."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList"))
        (code (prin1-to-string (cfg-test-read-forms))))
    ;; Assert
    (should-not (member "ligature" packages))
    (should-not (string-match-p "ligature" code))))

(provide 'font-faces-test)
;;; font-faces-test.el ends here
