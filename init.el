;;; init.el --- Literate config loader -*- lexical-binding: t; -*-
;;; Commentary:
;; Loads the literate configuration.  Emacs packages are provided by Nix
;; (see flake.nix / nix/hm-module.nix), so there is no package-manager
;; bootstrap here: use-package is built in to Emacs 29+, and packages are
;; already on `load-path'.  This file only tangles and loads config.org.
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; Packages come from Nix; never let use-package try to install anything.
(require 'use-package)
(setq use-package-always-ensure nil)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
