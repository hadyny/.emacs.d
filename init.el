;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-enable-at-startup nil) ;; Disables the default package manager.

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :straight t
  :config (setq exec-path-from-shell-variables '("PATH"))
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (create-lockfiles nil)
  (display-line-numbers-type 'relative)
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum t)
  (ring-bell-function 'ignore)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (inhibit-startup-screen t)

  (tsx-ts-mode-indent-offset 4)
  (typescript-ts-mode-indent-offset 4)

  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (indent-tabs-mode nil)
  (recentf-mode 1)
  (savehist-mode 1)
  (global-prettify-symbols-mode t)
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "open" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

(use-package eldoc-box
  :ensure t
  :straight t
  :defer t)

;; (defun ek/lsp-describe-and-jump ()
;;     (interactive)
;;     (lsp-describe-thing-at-point)
;;     (let ((help-buffer "*lsp-help*"))
;;       (when (get-buffer help-buffer)
;;         (switch-to-buffer-other-window help-buffer))))

;;   (evil-define-key 'normal 'global (kbd "K")
;;     (if (>= emacs-major-version 31)
;;         #'eldoc-box-help-at-point
;;         #'ek/lsp-describe-and-jump))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.05)
  (which-key-mode))

(use-package doom-themes
  :ensure t
  :straight t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package catppuccin-theme
;;   :ensure t
;;   :straight t
;;   :config
;;   (custom-set-faces
;;    ;; Set the color for changes in the diff highlighting to blue.
;;    `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))

;;   (custom-set-faces
;;    ;; Set the color for deletions in the diff highlighting to red.
;;    `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))

;;   (custom-set-faces
;;    ;; Set the color for insertions in the diff highlighting to green.
;;    `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green))))))

;;     ;; Load the Catppuccin theme without prompting for confirmation.
;;   (load-theme 'catppuccin :no-confirm))

(use-package nerd-icons
  :ensure t
  :straight t
  :custom
  (nerd-icons-font-family "Maple Mono NF"))

(set-face-attribute 'default nil
  :font "Maple Mono NF"
  :height 140
  :weight 'semibold)
(set-face-attribute 'variable-pitch nil
  :font "Maple Mono NF"
  :height 140
  :weight 'semibold)
(set-face-attribute 'fixed-pitch nil
  :font "Maple Mono NF"
  :height 140
  :weight 'semibold)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(modify-all-frames-parameters
 '((right-divider-width . 20)
   (internal-border-width . 20)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))


(use-package minions
  :ensure t
  :straight t
  :config
  (minions-mode t))

(use-package moody
  :ensure t
  :straight t
  :config
  (setq moody-mode-line-height 32)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package dirvish
  :ensure t
  :straight t
  :defer t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("s" "~/src/")                     "Source"
     ("e" "~/src/ep/"                   "EP")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  (setq dirvish-side-display-alist '((side . right) (slot . -1)))
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

(use-package vertico
  :ensure t
  :straight t
  :hook (after-init . vertico-mode)
  :init (vertico-mode)
  :custom
  (vertico-count 12)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-scroll-margin 0))

(use-package orderless
  :ensure t
  :straight t
  :after vertico
  :custom
  (completion-category-defaults nil)
  (completion-styles '(orderless partial-completion))
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :ensure t
  :straight t
  :hook ((org-mode
          typescript-ts-mode
          tsx-ts-mode
          csharp-ts-mode
          json-ts-mode
          emacs-lisp-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-info t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-quit-no-match t)
  (corfu-popup-delay 0.5)
  (corfu-preselect 'first)
  (corfu-preview-current 'insert)
  (corfu-completion-styles '(orderless))
  (text-mode-ispell-word-completion . nil)
  :config
   (setq corfu--frame-parameters
          '((no-accept-focus . t)
            (no-focus-on-map . t)
            (min-width . t)
            (min-height . t)
            (border-width . 0)
            (outer-border-width . 0)
            (internal-border-width . 1)
            (child-frame-border-width . 2)
            (left-fringe . 0)
            (right-fringe . 0)
            (vertical-scroll-bars)
            (horizontal-scroll-bars)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (tab-bar-lines . 0)
            (no-other-frame . t)
            (unsplittable . t)
            (undecorated . t)
            (cursor-type)
            (no-special-glyphs . t)
            (desktop-dont-save . t)))
  :init
  ;; HACK: this avoids corfu to hang while waiting for completions from lsp
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ([tab] . corfu-insert)
        ("ESC" . corfu-quit)
        ([esc] . corfu-quit)))

(use-package nerd-icons-corfu
  :ensure t
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :straight t
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package consult
  :ensure t
  :straight t
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t
  :straight t
  :defer t)

(use-package embark-consult
  :ensure t
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  :ensure
  :straight t
  :defer t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))


(use-package smartparens
  :defer t
  :ensure t
  :straight t
  :hook
  (prog-mode . smartparens-mode))

(use-package neotree
  :ensure t
  :straight t
  :config
  (setq neo-window-width 40)
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (setq neo-window-position 'right)

  (define-key neotree-mode-map (kbd "n") 'neotree-create-node)
  (define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
  (define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
  (define-key neotree-mode-map (kbd "c") 'neotree-copy-node)

  (global-set-key (kbd "C-x n")
                  (lambda ()
                    (interactive)
                    (neotree-dir (read-directory-name "Directory: ")))))

(use-package ligature
  :defer t
  :ensure t
  :straight t
  :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://")))

(use-package lsp-mode
  :ensure t
  :straight t
  :hook ((csharp-ts-mode
      json-ts-mode
      typescript-ts-mode
      tsx-ts-mode
      nix-mode) . lsp-deferred)
  :commands lsp lsp-deferred
  :custom
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-session-file "~/.emacs.d/.lsp-session-v1")

  (lsp-enable-xref t)
  (lsp-enable-links t)
  (lsp-enable-imenu nil)
  (lsp-enable-indentation nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-text-document-color t)
  (lsp-eldoc-render-all t)
  (lsp-inlay-hint-enable nil)

  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-delay 0)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-diagnostic-max-lines 20)

  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)

  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)

  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-icons-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)

  (lsp-completion-show-kind t)
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node_modules\\'")
  (setq lsp-clients-typescript-tls-path "vtsls")
  (setq lsp-clients-typescript-prefer-use-project-ts-server t)
  (setq lsp-typescript-format-enable nil)
  (setq lsp-typescript-implementations-code-lens-enabled t)
  (setq lsp-typescript-references-code-lens-enabled t)
  (setq lsp-enable-which-key-integration t)
  (setq lsp-eslint-working-directories [(mode "location")])

  :init
  (setq lsp-use-plists t))

(use-package lsp-ui
  :ensure t
  :straight t
  :after lsp-mode
  :config
  (setq lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "M-?") #'lsp-ui-peek-find-references))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode (OLD-FN ARGS) instead of JSON."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command (OLD-FN TEST?) to LSP CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command
        :around
        #'lsp-booster--advice-final-command)

(use-package lsp-tailwindcss
  :ensure t
  :straight t
  :defer t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-skip-config-check t)
  (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)
  :config
  (customize-set-value 'lsp-tailwindcss-class-attributes ["class" "className" "cn"])
  :custom
  (lsp-tailwindcss-server-path (executable-find "tailwindcss-language-server")))

(use-package lsp-eslint
  :config
  (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  :after lsp-mode)

(use-package nix-mode
  :ensure t
  :straight t
  :defer t
  :mode "\\.nix\\'")

(use-package flycheck
  :ensure t
  :straight t
  :defer t
  :init (global-flycheck-mode)
  :config

  ;; Show indicators in the left margin
  (setq flycheck-indication-mode 'left-margin)

  ;; Adjust margins and fringe widths…
  (defun my/set-flycheck-margins ()
    (setq left-fringe-width 8 right-fringe-width 8
          left-margin-width 1 right-margin-width 0)
    (flycheck-refresh-fringes-and-margins))
  ;; …every time Flycheck is activated in a new buffer
  (add-hook 'flycheck-mode-hook #'my/set-flycheck-margins))

;; (use-package flymake
;;   :defer t
;;   :ensure nil
;;   :hook
;;   (prog-mode . flymake-mode)
;;   :custom
;;   (flymake-show-diagnostics-at-end-of-line 'fancy)
;;   (flymake-indicator-type 'margins)
;;   (flymake-margin-indicators-string
;;    `((error "󰅙  " compilation-error)
;;      (warning "  " compilation-warning)
;;      (note "󰋼  " compilation-info)))
;;   :config
;;   (defun hy/toggle-flymake-inline-diagnostics ()
;;     "Toggle `flymake-show-diagnostics-at-end-of-line` between 'short and nil, and refresh Flymake."
;;     (interactive)
;;     (setq flymake-show-diagnostics-at-end-of-line
;;             (if (eq flymake-show-diagnostics-at-end-of-line 'short)
;;                     nil
;;               'short))
;;     ;; Refresh Flymake to apply the new setting
;;     (flymake-mode-off)
;;     (flymake-mode)
;;     (message "flymake-show-diagnostics-at-end-of-line is now %s"
;;                flymake-show-diagnostics-at-end-of-line))

;;   (defun hy/toggle-flymake-diagnostics ()
;;     "Toggle Flymake mode on or off."
;;     (interactive)
;;     (if flymake-mode
;;           (progn
;;             (flymake-mode-off)
;;             (message "Flymake mode is now OFF"))
;;         (flymake-mode)
;;         (message "Flymake mode is now ON")))

;;   (bind-keys :map flymake-mode-map
;;                ;; ("C-c ! l" . flymake-show-buffer-diagnostics)
;;                ("C-c ! l" . consult-flymake)
;;                ("C-c ! P" . flymake-show-project-diagnostics)
;;                ("C-c ! n" . flymake-goto-next-error)
;;                ("C-c ! p" . flymake-goto-prev-error)
;;                ("C-c ! i" . hy/toggle-flymake-inline-diagnostics)
;;                ("C-c ! d" . hy/toggle-flymake-diagnostics)
;;                ("M-7" . flymake-goto-prev-error)
;;                ("M-8" . flymake-goto-next-error)))

(use-package flyover
  :defer t
  :straight (flyover :type git :host github :repo "konrad1977/flyover")
  :ensure t
  :hook (flycheck-mode-hook . flyover-mode)
  :config
  (setq flyover-levels '(error warning info))
  (setq flyover-use-theme-colors t)
  (setq flyover-checkers '(flycheck flymake))
  (setq flyover-background-lightness 30)
  (setq flyover-wrap-messages t)
  (setq flyover-max-line-length 80)
  (setq flyover-hide-checker-name t))


(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))

(use-package org
  :ensure t
  :defer t
  :config
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-agenda-files '("~/org/"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t))))

(use-package magit
  :ensure t
  :straight t
  :defer t)

(use-package add-node-modules-path
  :ensure t
  :straight t
  :defer t
  :custom
  ;; Makes sure you are using the local bin for your
  ;; node project. Local eslint, typescript server...
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescriptreact-mode
    '(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
     "276228257774fa4811da55346b1e34130edb068898565ca07c2d83cfb67eb70a"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background unspecified :foreground "#89b4fa"))))
 '(diff-hl-delete ((t (:background unspecified :foreground "#f38ba8"))))
 '(diff-hl-insert ((t (:background unspecified :foreground "#a6e3a1")))))
(provide 'init)
;;; init.el ends here
