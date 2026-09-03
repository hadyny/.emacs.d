;;; eglot-watch-glob-test.el --- Tests for tolerant watch-glob compiling -*- lexical-binding: t; -*-

;; Eglot's glob grammar leaves the comma out of its literal character class:
;;
;;   (:literal "[^][,*?{}]+" eglot--glob-emit-self)
;;
;; A comma is only special inside brace expansion, `{a,b}', so a filename that
;; contains one cannot be parsed at all.  Roslyn watches the MSBuild-generated
;; `obj/.../.NETCoreApp,Version=v10.0.AssemblyAttributes.cs', and every connect
;; then reports:
;;
;;   Error running timer: (error "Glob '.NETCoreApp,Version=v10.0.…' invalid at 12")
;;
;; The damage is a little wider than one skipped file.  `eglot--glob-compile'
;; parses eagerly, before its own NOERROR argument can apply -- NOERROR only
;; wraps the *generated* matcher.  So the error escapes the `mapc' inside the
;; `workspace/didChangeWatchedFiles' registration, and the `maphash' that
;; actually creates the watches runs after that `mapc'.  Every glob in the same
;; registration is lost, not just the bad one.  Roslyn sends several
;; registrations, so the rest survive: measured on a one-project solution, 137
;; watches without the advice and 140 with it.
;;
;; The advice makes one unparseable glob skip itself instead of taking its
;; registration down.  A skipped glob is the right outcome here: the file in
;; question is a generated build artefact.
;;
;; Separately, `eglot-max-file-watches' is lowered from Eglot's own default of
;; 10000.  That default is effectively no cap: one Roslyn solution alone
;; registers ~140 watches, and `handle-event' keeps adding more for the life
;; of the connection as matching directories are (re)created -- a `dotnet
;; build' regenerating `obj/...', an `npm install' unpacking `node_modules'.
;; Left uncapped this can exhaust the process's OS-level descriptor budget,
;; surfacing as `file-notify-error "File watching not possible, no file
;; descriptor left"' wherever something else next needed a descriptor.
;;
;; A fixed guess for the cap is the wrong shape: 500 turned out lower than a
;; real machine's descriptor limit could support, so it started rejecting
;; watches Eglot could safely have created ("Reached `eglot-max-file-watches'
;; limit of 500, not watching some directories").  `my/eglot-max-file-watches-for-limit'
;; derives the cap from this process's actual `ulimit -n' instead (there is
;; no `getrlimit' binding, but a subprocess inherits the same limit via
;; fork/exec), minus fixed headroom for everything else needing a descriptor.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defconst ewg-test--roslyn-glob
  "**/obj/.NETCoreApp,Version=v10.0.AssemblyAttributes.cs"
  "The generated file Roslyn asks Eglot to watch, which Eglot cannot parse.")

(defun ewg-test--config-loaded-p ()
  "Return non-nil if this Emacs has loaded the full configuration."
  (memq 'my/add-node-modules-path tsx-ts-mode-hook))

;;; Pure -- the advice itself

(ert-deftest eglot-watch-glob/a-good-glob-passes-through ()
  "A glob that compiles is handed back untouched, with every argument."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-glob-compile-tolerantly)
  (let (seen)
    ;; Act
    (let ((result (my/eglot-glob-compile-tolerantly
                   (lambda (&rest args) (setq seen args) 'compiled)
                   "*.cs" t t)))
      ;; Assert
      (should (eq result 'compiled))
      (should (equal seen '("*.cs" t t))))))

(ert-deftest eglot-watch-glob/an-unparseable-glob-is-skipped ()
  "A glob that will not compile yields a matcher instead of an error.
The registration must survive so the other watches in the batch are created."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-glob-compile-tolerantly)
  ;; Act -- the stub fails the way `eglot--glob-parse' does.
  (let ((result (my/eglot-glob-compile-tolerantly
                 (lambda (&rest _) (error "Glob '%s' invalid at 12" "x,y"))
                 "x,y" t t)))
    ;; Assert
    (should (functionp result))
    (should-not (funcall result "x,y"))
    (should-not (funcall result "anything at all"))))

(ert-deftest eglot-watch-glob/skipped-glob-honours-byte-compile ()
  "The replacement matcher is returned in the shape the caller asked for.
`eglot--glob-compile' returns a byte-compiled function or a bare lambda form
depending on its BYTE-COMPILE argument, and callers rely on that."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-glob-compile-tolerantly)
  (let ((thrower (lambda (&rest _) (error "Glob invalid"))))
    ;; Act / Assert -- both shapes must be callable and match nothing.
    (dolist (byte-compile '(nil t))
      (let ((result (my/eglot-glob-compile-tolerantly thrower "x,y" byte-compile t)))
        (should (functionp result))
        (should-not (funcall result "x,y"))))
    ;; Byte-compiling actually happened when it was asked for.
    (should (byte-code-function-p
             (my/eglot-glob-compile-tolerantly thrower "x,y" t t)))))

;;; Structural / behavioural -- wired up against the real Eglot

(ert-deftest eglot-watch-glob/advice-is-installed ()
  "The advice is attached to Eglot's glob compiler."
  ;; Arrange
  (skip-unless (ewg-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Assert
  (should (advice-member-p #'my/eglot-glob-compile-tolerantly
                           'eglot--glob-compile)))

(ert-deftest eglot-watch-glob/roslyn-glob-no-longer-signals ()
  "Eglot compiles Roslyn's comma-bearing glob without signalling.
This is the exact pattern behind the \"invalid at 12\" error on every connect."
  ;; Arrange
  (skip-unless (ewg-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Act
  (let ((matcher (eglot--glob-compile ewg-test--roslyn-glob t t)))
    ;; Assert
    (should (functionp matcher))))

(ert-deftest eglot-watch-glob/ordinary-globs-still-match ()
  "The advice does not blunt Eglot's own matching for globs that do compile."
  ;; Arrange
  (skip-unless (ewg-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Act
  (let ((matcher (eglot--glob-compile "**/*.cs" t t)))
    ;; Assert
    (should (funcall matcher "src/deep/Thing.cs"))
    (should-not (funcall matcher "src/Thing.fs"))))

;;; File watch descriptor budget

(ert-deftest eglot-watch-glob/parse-file-descriptor-limit-reads-a-number ()
  "A plain numeric `ulimit -n' output parses to that number."
  ;; Arrange
  (cfg-test-load-defun 'my/parse-file-descriptor-limit)
  ;; Act / Assert -- trailing newline, as `shell-command-to-string' leaves it.
  (should (equal (my/parse-file-descriptor-limit "4096\n") 4096)))

(ert-deftest eglot-watch-glob/parse-file-descriptor-limit-treats-unlimited-as-nil ()
  "\"unlimited\" (no numeric ceiling) parses to nil, not a bogus number."
  ;; Arrange
  (cfg-test-load-defun 'my/parse-file-descriptor-limit)
  ;; Act / Assert
  (should-not (my/parse-file-descriptor-limit "unlimited\n")))

(ert-deftest eglot-watch-glob/max-file-watches-for-limit-reserves-headroom ()
  "A known LIMIT becomes that limit minus headroom for everything else.
Corfu's child frames, every LSP/Flycheck/Magit subprocess pipe, and D-Bus all
need a descriptor too, so the cap must leave some of the real limit unclaimed
by Eglot's own watches."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-max-file-watches-for-limit)
  ;; Act / Assert
  (should (equal (my/eglot-max-file-watches-for-limit 4096) 3584)))

(ert-deftest eglot-watch-glob/max-file-watches-for-limit-has-a-floor ()
  "A genuinely low LIMIT still leaves room for a couple of Roslyn solutions.
Reserving a fixed headroom from a low limit could otherwise cap below what
one project alone needs (~140 watches, measured above), rejecting watches
Eglot could safely have created instead of just warning once."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-max-file-watches-for-limit)
  ;; Act / Assert
  (should (equal (my/eglot-max-file-watches-for-limit 600) 500))
  (should (equal (my/eglot-max-file-watches-for-limit 100) 500)))

(ert-deftest eglot-watch-glob/max-file-watches-for-limit-unlimited-is-generous ()
  "A nil LIMIT (unlimited/unknown) still gets a real, generous cap.
`eglot-max-file-watches' must never be left nil/unset: that reads as \"no
limit\" to Eglot's own guard, the exact unbounded default this exists to fix."
  ;; Arrange
  (cfg-test-load-defun 'my/eglot-max-file-watches-for-limit)
  ;; Act / Assert
  (should (equal (my/eglot-max-file-watches-for-limit nil) 4000)))

(ert-deftest eglot-watch-glob/max-file-watches-guard-is-live ()
  "The cap is in force once Eglot has actually loaded, derived from a real
limit rather than left at Eglot's own unbounded default."
  ;; Arrange
  (skip-unless (ewg-test--config-loaded-p))
  (should (require 'eglot nil t))
  ;; Assert
  (should (integerp eglot-max-file-watches))
  (should (< eglot-max-file-watches 10000))
  (should (>= eglot-max-file-watches 500)))

(provide 'eglot-watch-glob-test)
;;; eglot-watch-glob-test.el ends here
