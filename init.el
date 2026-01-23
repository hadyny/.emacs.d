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
  (global-font-lock-mode t)
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
          nix-mode
          lua-ts-mode
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

(use-package nix-mode
  :ensure t
  :straight t
  :defer t
  :mode "\\.nix\\'")

;; (use-package flycheck
;;   :ensure t
;;   :straight t
;;   :defer t
;;   :init (global-flycheck-mode)
;;   :config

  ;; ;; Show indicators in the left margin
  ;; (setq flycheck-indication-mode 'left-margin)
  ;; (setq flycheck-javascript-eslint-executable "eslint_d")

  ;; ;; Adjust margins and fringe widths…
  ;; (defun my/set-flycheck-margins ()
  ;;   (setq left-fringe-width 8 right-fringe-width 8
  ;;         left-margin-width 1 right-margin-width 0)
  ;;   (flycheck-refresh-fringes-and-margins))
  ;; ;; …every time Flycheck is activated in a new buffer
  ;; (add-hook 'flycheck-mode-hook #'my/set-flycheck-margins))

(use-package flymake
  :defer t
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "󰅙  " compilation-error)
     (warning "  " compilation-warning)
     (note "󰋼  " compilation-info)))
  :config
  (defun hy/toggle-flymake-inline-diagnostics ()
    "Toggle `flymake-show-diagnostics-at-end-of-line` between 'short and nil, and refresh Flymake."
    (interactive)
    (setq flymake-show-diagnostics-at-end-of-line
            (if (eq flymake-show-diagnostics-at-end-of-line 'short)
                    nil
              'short))
    ;; Refresh Flymake to apply the new setting
    (flymake-mode-off)
    (flymake-mode)
    (message "flymake-show-diagnostics-at-end-of-line is now %s"
               flymake-show-diagnostics-at-end-of-line))

  (defun hy/toggle-flymake-diagnostics ()
    "Toggle Flymake mode on or off."
    (interactive)
    (if flymake-mode
          (progn
            (flymake-mode-off)
            (message "Flymake mode is now OFF"))
        (flymake-mode)
        (message "Flymake mode is now ON")))

  (bind-keys :map flymake-mode-map
               ;; ("C-c ! l" . flymake-show-buffer-diagnostics)
               ("C-c ! l" . consult-flymake)
               ("C-c ! P" . flymake-show-project-diagnostics)
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! p" . flymake-goto-prev-error)
               ("C-c ! i" . hy/toggle-flymake-inline-diagnostics)
               ("C-c ! d" . hy/toggle-flymake-diagnostics)
               ("M-7" . flymake-goto-prev-error)
               ("M-8" . flymake-goto-next-error)))

;; (use-package flyover
;;   :defer t
;;   :straight (flyover :type git :host github :repo "konrad1977/flyover")
;;   :ensure t
;;   :hook (flycheck-mode-hook . flyover-mode)
;;   :config
;;   (setq flyover-levels '(error warning info))
;;   (setq flyover-use-theme-colors t)
;;   (setq flyover-checkers '(flycheck flymake))
;;   (setq flyover-background-lightness 30)
;;   (setq flyover-wrap-messages t)
;;   (setq flyover-max-line-length 80)
;;   (setq flyover-hide-checker-name t))


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
        org-auto-align-tags nil
        org-tags-column 0
        org-fold-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-ellipsis "…"
        org-agenda-files '("~/org/"))

  (setq +org-capture-projects-file (expand-file-name "projects.org" (car org-agenda-files))
        +org-capture-todo-file     (expand-file-name "todo.org"    (car org-agenda-files))
        +org-capture-journal-file  (expand-file-name "schedule.org" (car org-agenda-files))
        +org-capture-notes-file    (expand-file-name "notes.org"    (car org-agenda-files)))

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  (setq org-capture-templates '(("t" "Quick todo" entry
                                 (file+headline +org-capture-todo-file "Todos")
                                 "* TODO %?\n%i\n" :prepend t)
                                ("e" "Event" entry
                                 (file+olp +org-capture-journal-file "Events")
                                 "* EVENT %?\n%i\n" :prepend nil)
                                ("i" "Random idea" entry
                                 (file+olp +org-capture-notes-file "Inbox")
                                 "* IDEA %?\n%i\n" :prepend t)
                                ("p" "Centralized templates for projects")
                                ("pt" "Project todo" entry
                                 #'+org-capture-central-project-todo-file
                                 "* TODO %?\n %i\n %a" :heading "Tasks" :prepend t)
                                ("pe""Project event" entry
                                 #'+org-capture-central-project-todo-file
                                 "* EVENT %?\n %i\n" :heading "Events" :prepend nil)
                                ("pi" "Project idea" entry
                                 #'+org-capture-central-project-todo-file
                                 "* IDEA %?\n %i\n %a" :heading "Ideas" :prepend t))))

;; (use-package org-super-agenda
;;   :ensure t
;;   :straight t
;;   :defer t
;;   :config
;;   (let ((org-super-agenda-groups
;;        '((:auto-group t))))
;;     (org-agenda-list)))

(use-package org-modern
  :ensure t
  :straight t
  :defer t
  :hook (org-mode . org-modern-mode))

(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
               '((csharp-ts-mode csharp-mode) . ("csharp-language-server")))
    (add-to-list 'eglot-server-programs
               '((tsx-ts-mode typescript-ts-mode) . ("rass"
                                                     "--" "typescript-language-server" "--stdio"
                                                     "--" "vscode-eslint-language-server" "--stdio"
                                                     "--" "tailwindcss-language-server" "--stdio"))))

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


;; EVIL
;; The `evil' package provides Vim emulation within Emacs, allowing
;; users to edit text in a modal way, similar to how Vim
;; operates. This setup configures `evil-mode' to enhance the editing
;; experience.
(use-package evil
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t)      ;; Integrate `evil' with other Emacs features (optional as it's true by default).
  (setq evil-want-keybinding nil)     ;; Disable default keybinding to set custom ones.
  (setq evil-want-C-u-scroll t)       ;; Makes C-u scroll
  (setq evil-want-C-u-delete t)       ;; Makes C-u delete on insert mode
  :config
  (evil-set-undo-system 'undo-tree)   ;; Uses the undo-tree package as the default undo system

  ;; Set the leader key to space for easier access to custom commands. (setq evil-want-leader t)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.
  (setq evil-want-fine-undo t)        ;; Evil uses finer grain undoing steps

  ;; Define the leader key as Space
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Keybindings for searching and finding files.
  (evil-define-key 'normal 'global (kbd "<leader> s f") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> s g") 'consult-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s G") 'consult-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s r") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

  ;; Flymake navigation
  (evil-define-key 'normal 'global (kbd "<leader> x x") 'consult-flymake);; Gives you something like `trouble.nvim'
  (evil-define-key 'normal 'global (kbd "] d") 'flymake-goto-next-error) ;; Go to next Flymake error
  (evil-define-key 'normal 'global (kbd "[ d") 'flymake-goto-prev-error) ;; Go to previous Flymake error

  ;; Dired commands for file management
  (evil-define-key 'normal 'global (kbd "<leader> x d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader> x j") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader> x f") 'find-file)

  ;; Diff-HL navigation for version control
  (evil-define-key 'normal 'global (kbd "] c") 'diff-hl-next-hunk) ;; Next diff hunk
  (evil-define-key 'normal 'global (kbd "[ c") 'diff-hl-previous-hunk) ;; Previous diff hunk

  ;; NeoTree command for file exploration
  (evil-define-key 'normal 'global (kbd "<leader> e e") 'neotree-toggle)
  (evil-define-key 'normal 'global (kbd "<leader> e d") 'dired-jump)

  ;; Magit keybindings for Git integration
  (evil-define-key 'normal 'global (kbd "<leader> g g") 'magit-status)      ;; Open Magit status
  (evil-define-key 'normal 'global (kbd "<leader> g l") 'magit-log-current) ;; Show current log
  (evil-define-key 'normal 'global (kbd "<leader> g d") 'magit-diff-buffer-file) ;; Show diff for the current file
  (evil-define-key 'normal 'global (kbd "<leader> g D") 'diff-hl-show-hunk) ;; Show diff for a hunk
  (evil-define-key 'normal 'global (kbd "<leader> g b") 'vc-annotate)       ;; Annotate buffer with version control info

  ;; Buffer management keybindings
  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer) ;; Switch to next buffer
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer) ;; Switch to previous buffer
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'consult-buffer) ;; Open consult buffer list
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'ibuffer) ;; Open Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer
  (evil-define-key 'normal 'global (kbd "<leader> b l") 'consult-buffer) ;; Consult buffer
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer) ;; Consult buffer

  ;; Project management keybindings
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer) ;; Consult project buffer
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project) ;; Switch project
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file) ;; Find file in project
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp) ;; Find regexp in project
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Orgmode keybindings
  (evil-define-key 'normal 'global (kbd "<leader> o a") 'org-agenda) ;; Org agenda
  (evil-define-key 'normal 'global (kbd "<leader> o p") 'org-capture) ;; Org capture

  ;; Yank from kill ring
  (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'consult-yank-from-kill-ring)

  ;; Embark actions for contextual commands
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)

  ;; Undo tree visualization
  (evil-define-key 'normal 'global (kbd "<leader> u") 'undo-tree-visualize)

  ;; Help keybindings
  (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode) ;; Describe current mode
  (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function) ;; Describe function
  (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable) ;; Describe variable
  (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key) ;; Describe key

  ;; Tab navigation
  (evil-define-key 'normal 'global (kbd "] t") 'tab-next) ;; Go to next tab
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous) ;; Go to previous tab

  ;; LSP commands keybindings
  (evil-define-key 'normal lsp-mode-map
                   ;; (kbd "gd") 'lsp-find-definition                ;; evil-collection already provides gd
                   (kbd "gr") 'lsp-find-references                   ;; Finds LSP references
                   (kbd "<leader> c a") 'lsp-execute-code-action     ;; Execute code actions
                   (kbd "<leader> r n") 'lsp-rename                  ;; Rename symbol
                   (kbd "gI") 'lsp-find-implementation               ;; Find implementation
                   (kbd "<leader> l f") 'lsp-format-buffer)          ;; Format buffer via lsp


  (defun hy/lsp-describe-and-jump ()
    "Show hover documentation and jump to *lsp-help* buffer."
    (interactive)
    (lsp-describe-thing-at-point)
    (let ((help-buffer "*lsp-help*"))
      (when (get-buffer help-buffer)
        (switch-to-buffer-other-window help-buffer))))

  ;; Emacs 31 finaly brings us support for 'floating windows' (a.k.a. "child frames")
  ;; to terminal Emacs. If you're still using 30, docs will be shown in a buffer at the
  ;; inferior part of your frame.
  (evil-define-key 'normal 'global (kbd "K")
    (if (>= emacs-major-version 31)
        #'eldoc-box-help-at-point
        #'hy/lsp-describe-and-jump))

  ;; Commenting functionality for single and multiple lines
  (evil-define-key 'normal 'global (kbd "gcc")
                   (lambda ()
                     (interactive)
                     (if (not (use-region-p))
                         (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

  (evil-define-key 'visual 'global (kbd "gc")
                   (lambda ()
                     (interactive)
                     (if (use-region-p)
                         (comment-or-uncomment-region (region-beginning) (region-end)))))

  ;; Enable evil mode
  (evil-mode 1))


;; EVIL COLLECTION
;; The `evil-collection' package enhances the integration of
;; `evil-mode' with various built-in and third-party packages. It
;; provides a better modal experience by remapping keybindings and
;; commands to fit the `evil' style.
(use-package evil-collection
  :defer t
  :straight t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))

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
