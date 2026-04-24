;;; init.el --- Literate config loader -*- lexical-binding: t; -*-
;;; Commentary:
;; Tangles and loads config.org.  All configuration lives there.
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
