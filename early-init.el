;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; This file is loaded before init.el, before package and UI initialization.
;; Used for performance optimizations.
;;; Code:

;; Keep package.el enabled: Nix installs Emacs packages into an `elpa'
;; directory on `package-directory-list', and `package-activate-all' (run at
;; startup only when this is non-nil) is what loads their autoloads. Disabling
;; it leaves every package function void (e.g. gcmh-mode, marginalia-mode).
(setq package-enable-at-startup t)

;; Increase garbage collection threshold for faster startup. `gcmh' (configured
;; in config.org) takes over after startup, keeping the threshold small while
;; idle and large during activity, so this high value is only for init.
(setq gc-cons-threshold #x40000000  ; 1GB
      gc-cons-percentage 0.6)

;; Increase read chunk size for better LSP performance
(setq read-process-output-max (* 1024 1024 4))  ; 4MB

;;; early-init.el ends here
