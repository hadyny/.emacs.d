;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; This file is loaded before init.el, before package and UI initialization.
;; Used for performance optimizations and disabling package.el.
;;; Code:

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; LSP performance: use plists instead of hash tables (for lsp-mode)
(setenv "LSP_USE_PLISTS" "true")

;; Increase garbage collection threshold for faster startup
;; Reset to lower value after init in init.el if needed
(setq gc-cons-threshold #x40000000  ; 1GB
      gc-cons-percentage 0.6)

;; Increase read chunk size for better LSP performance
(setq read-process-output-max (* 1024 1024 4))  ; 4MB

;;; early-init.el ends here
