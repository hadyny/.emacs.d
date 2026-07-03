;;; auto-dark-detectable-test.el --- Tests for the auto-dark detection guard -*- lexical-binding: t; -*-

;; Unit tests for `my/auto-dark-detectable-p', the predicate that decides
;; whether a viable system-appearance detection mechanism exists before we
;; enable `auto-dark-mode'.  Without it, `auto-dark-mode' logs
;; "Could not determine a viable theme detection mechanism!" at startup on
;; hosts that cannot detect appearance (e.g. a Linux TTY with no session bus,
;; or a macOS terminal with osascript disabled).
;;
;; The predicate is extracted in isolation from the tangled `config.el' so the
;; test does not have to load the whole configuration (fonts, themes, package
;; modes) — mirroring the repo's "pure functions, tested in isolation" style.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

;; `auto-dark-allow-osascript' is a defcustom from the auto-dark package, which
;; is not loaded here. Declare it special so `let' bindings below are dynamic
;; and visible to `bound-and-true-p' inside the function under test.
(defvar auto-dark-allow-osascript)

(ert-deftest auto-dark-detectable/linux-no-session-bus ()
  "On a Linux TTY with no session bus, no mechanism is viable."
  ;; Arrange
  (cfg-test-load-defun 'my/auto-dark-detectable-p)
  (cl-letf (((symbol-function 'dbus-list-activatable-names)
             (lambda (&rest _) (error "no session bus"))))
    (let ((system-type 'gnu/linux))
      ;; Act
      (let ((result (my/auto-dark-detectable-p)))
        ;; Assert
        (should-not result)))))

(ert-deftest auto-dark-detectable/linux-portal-present ()
  "On Linux with the desktop portal on the session bus, dbus is viable."
  ;; Arrange
  (skip-unless (require 'dbus nil t))
  (cfg-test-load-defun 'my/auto-dark-detectable-p)
  (cl-letf (((symbol-function 'dbus-list-activatable-names)
             (lambda (&rest _) '("org.freedesktop.portal.Desktop"))))
    (let ((system-type 'gnu/linux))
      ;; Act
      (let ((result (my/auto-dark-detectable-p)))
        ;; Assert
        (should result)))))

(ert-deftest auto-dark-detectable/darwin-terminal-osascript ()
  "On a macOS terminal with osascript allowed and present, detection is viable."
  ;; Arrange
  (cfg-test-load-defun 'my/auto-dark-detectable-p)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) "/usr/bin/osascript")))
    (let ((system-type 'darwin)
          (window-system nil)
          (auto-dark-allow-osascript t))
      ;; Act
      (let ((result (my/auto-dark-detectable-p)))
        ;; Assert
        (should result)))))

(ert-deftest auto-dark-detectable/darwin-terminal-osascript-disabled ()
  "On a macOS terminal with osascript disabled, no mechanism is viable."
  ;; Arrange
  (cfg-test-load-defun 'my/auto-dark-detectable-p)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (&rest _) "/usr/bin/osascript")))
    (let ((system-type 'darwin)
          (window-system nil)
          (auto-dark-allow-osascript nil))
      ;; Act
      (let ((result (my/auto-dark-detectable-p)))
        ;; Assert
        (should-not result)))))

(ert-deftest auto-dark-detectable/darwin-gui ()
  "On a macOS GUI frame, the applescript mechanism is viable."
  ;; Arrange
  (cfg-test-load-defun 'my/auto-dark-detectable-p)
  (let ((system-type 'darwin)
        (window-system 'ns))
    ;; Act
    (let ((result (my/auto-dark-detectable-p)))
      ;; Assert
      (should result))))

(provide 'auto-dark-detectable-test)
;;; auto-dark-detectable-test.el ends here
