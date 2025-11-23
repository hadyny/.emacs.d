;;; early-init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setenv "LSP_USE_PLISTS" "true")

(setq gc-cons-threshold #x40000000
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 1024 1024 4))

;;; early-init.el ends here
