;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Main Emacs configuration using straight.el and use-package.
;; Organized into sections: Package Management, Environment, UI, Completion,
;; Navigation, Editing, Languages, Version Control, Org-mode, LSP, and Evil.
;;; Code:

;;;; Package Management - straight.el bootstrap
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

;;;; Environment - Shell and system integration

(defconst *is-a-mac* (eq system-type 'darwin))

(use-package exec-path-from-shell
  :ensure t
  :straight t
  :config (setq exec-path-from-shell-variables '("PATH"))
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Mouse in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;;;; UI and Themes
;; Apply to all new frames (including the initial one if in early-init.el)
(add-to-list 'default-frame-alist '(min-height             . 1))
(add-to-list 'default-frame-alist '(height                 . 45))   ; adjust to taste
(add-to-list 'default-frame-alist '(min-width              . 1))
(add-to-list 'default-frame-alist '(width                  . 100))   ; adjust to taste
(add-to-list 'default-frame-alist '(internal-border-width  . 18)) ; ← main padding!
(add-to-list 'default-frame-alist '(left-fringe            . 1))
(add-to-list 'default-frame-alist '(right-fringe           . 1))

;; Optional: fallback glyph for truncation/wrap (clean look)
(require 'disp-table)
(defface my-fallback-glyph
  '((t :family "Maple Mono NF"  ; or Fira Code / whatever you use
       :inherit font-lock-comment-face))   ; or any faded face
  "Fallback face for truncation and wrap glyphs.")

(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'my-fallback-glyph))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'my-fallback-glyph))  ; or ? or ?…


(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (create-lockfiles nil)
  (display-line-numbers-type 'relative)
  (make-backup-files nil)
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
  (indent-tabs-mode nil)
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (recentf-mode 1)
  (savehist-mode 1)
  (global-prettify-symbols-mode 1)
  (global-font-lock-mode 1)
  (pixel-scroll-precision-mode 1))

(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :custom
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

(use-package catppuccin-theme
  :ensure t
  :straight t
  :config
  (defun my/apply-catppuccin-flavor (appearance)
    "Set catppuccin flavor based on system APPEARANCE (dark or light)."
    (setq catppuccin-flavor (if (eq appearance 'light) 'latte 'mocha))
    (catppuccin-reload))
  (add-hook 'ns-system-appearance-change-functions #'my/apply-catppuccin-flavor)
  (my/apply-catppuccin-flavor ns-system-appearance))

(use-package doom-themes
  :ensure t
  :straight t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'catppuccin t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
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

(set-face-background 'fringe (face-attribute 'default :background))

;;;; Tree-sitter - Modern syntax highlighting

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))


;;;; Mode-line - minions and moody

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

;;;; Completion - Vertico, Orderless, Corfu, Consult, Marginalia

(use-package vertico
  :ensure t
  :straight t
  :hook (after-init . vertico-mode)
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
  :hook ((org-mode prog-mode) . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-quit-no-match t)
  (corfu-popupinfo-delay 0.5)
  (corfu-preselect 'first)
  (corfu-preview-current 'insert)
  (text-mode-ispell-word-completion nil)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
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

(use-package marginalia
  :ensure t
  :straight t
  :defer t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;; Actions - Embark

(use-package embark
  :ensure t
  :straight t
  :defer t)

(use-package embark-consult
  :ensure t
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Help - Enhanced help system

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

;;;; Editing - Smartparens and Markdown

(use-package smartparens
  :defer t
  :ensure t
  :straight t
  :hook
  (prog-mode . smartparens-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :straight t
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;; File Explorer - Neotree

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

;;;; Ligatures - Font ligature support

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


;;;; Languages - Nix

(use-package nix-mode
  :ensure t
  :straight t
  :defer t
  :mode "\\.nix\\'")

;;;; Diagnostics - Flymake

(use-package flymake
  :defer t
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-margin-indicators-string
   `((error "󰅙 " compilation-error)
     (warning " " compilation-warning)
     (note "󰋼 " compilation-info))))

;;;; Version Control - diff-hl

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

;;;; Org-mode - Notes, agenda, and capture

(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-startup-indented t
        org-log-done 'time
        org-hide-emphasis-markers t
        org-auto-align-tags nil
        org-tags-column 0
        org-fold-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-ellipsis "…"
        org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

  (setq +org-capture-projects-file "~/org/projects.org"
        +org-capture-todo-file     "~/org/todo.org"
        +org-capture-journal-file  "~/org/schedule.org"
        +org-capture-notes-file    "~/org/notes.org")

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "WONT-DO")))

  (setq org-capture-templates '(
                                ("t" "Quick todo" entry
                                 (file+headline +org-capture-todo-file "Todos")
                                 "* TODO %?\n:Created: %T\n" :prepend t)
                                ("c" "Code To-Do"
                                 entry (file+headline "~/org/todos.org" "Code Related Tasks")
                                 "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
                                 :empty-lines 0)
                                ("e" "Event")
                                ("er" "Recurring Event" entry
                                 (file+olp +org-capture-journal-file "Events")
                                 "** EVENT %?\n%i\n" :heading "Recurring Events" :prepend nil)
                                ("eo" "One-off Event" entry
                                 (file+olp +org-capture-journal-file "Events")
                                 "** EVENT %?\n%i\n" :heading "One-off Events" :prepend nil)
                                ("w" "Work Log Entry" entry
                                 (file+datetree "~/org/work-log.org")
                                 "* %?" :empty-lines 0)
                                ("n" "Note" entry
                                 (file+headline "~/org/notes.org" "Random Notes")
                                 "** %?" :empty-lines 0)))

  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "GoldenRod" :weight bold))
          ("PLANNING" . (:foreground "DeepPink" :weight bold))
          ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
          ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
          ("BLOCKED" . (:foreground "Red" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("OBE" . (:foreground "LimeGreen" :weight bold))
          ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
          ))

  (setq org-tag-faces
        '(
          ("work"      . (:foreground "mediumPurple1" :weight bold))
          ("config"    . (:foreground "royalblue1"    :weight bold))
          ("personal"  . (:foreground "forest green"  :weight bold))
          ;; ("QA"        . (:foreground "sienna"        :weight bold))
          ;; ("meeting"   . (:foreground "yellow1"       :weight bold))
          ;; ("CRITICAL"  . (:foreground "red1"          :weight bold))
          )
        )

  ;; Agenda View "d"
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

    PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (setq org-agenda-skip-deadline-if-done t)

  (setq org-agenda-custom-commands
        '(
          ("c" "Current Work"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High Priority:")))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "Other TODOs:")))
            (agenda ""
                    ((org-agenda-span 3)
                     (org-agenda-start-day "-1d")
                     (org-agenda-overriding-header "Previous Work:")))))

          ("S" "Standup"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day "-1d")
                     (org-agenda-overriding-header "Yesterday:")))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "Currently Working On:")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Blocked Items:")))
            (tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High Priority TODOs:")))))

          ("w" "Week Overview"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-overriding-header "This Week:")))))

          ("o" "Combined Overview"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High Priority:")))
            (agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-overriding-header "Today:")))
            (tags-todo "work"
                       ((org-agenda-overriding-header "Work Tasks:")))
            (tags-todo "personal"
                       ((org-agenda-overriding-header "Personal Tasks:")))))

          ("d" "Daily agenda and all TODOs"

           ;; Display items with priority A
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))

            ;; View 7 days in the calendar view
            (agenda "" ((org-agenda-span 7)))

            ;; Display items with priority B (really it is view all items minus A & C)
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                     (air-org-skip-subtree-if-priority ?C)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:")))

            ;; Display items with pirority C
            (tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Low-priority Unfinished tasks:")))


            (todo "DONE" ((org-agenda-overriding-header "Done Tasks")))

            )

           ;; Don't compress things (change to suite your tastes)
           ((org-agenda-compact-blocks nil)))
          )))

(use-package org-modern
  :ensure t
  :straight t
  :defer t
  :hook (org-mode . org-modern-mode))

;;;; LSP - Eglot language server

(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :hook
  (csharp-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (nix-mode . eglot-ensure)
  :config
  (setq eglot-code-action-indications '(mode-line))
  (setq eglot-workspace-configuration
        '(:eslint (:validate "on"
                             :workingDirectory (:mode "auto"))
                  :tailwindCSS (:classAttributes ["class" "className" "cn"])))
  (add-to-list 'eglot-server-programs
               '((csharp-ts-mode csharp-mode) . ("csharp-language-server")))
  (add-to-list 'eglot-server-programs
               '((tsx-ts-mode typescript-ts-mode) . ("rass" "tslint"))))

;;;; Formatting - Apheleia

(use-package apheleia
  :ensure t
  :straight t
  :defer t
  :hook (after-init . apheleia-global-mode))

;;;; Git - Magit

(use-package magit
  :ensure t
  :straight t
  :defer t)

;;;; JavaScript/TypeScript - Node modules path

(use-package add-node-modules-path
  :ensure t
  :straight t
  :hook
  ((tsx-ts-mode typescript-ts-mode js-mode) . add-node-modules-path))

;;;; Terminal - Vterm

(use-package vterm
  :ensure t
  :straight t
  :defer t)

;;;; Evil - Vim emulation

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
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'consult-buffer) ;; Buffer list
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'ibuffer) ;; Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill buffer
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer) ;; Quick buffer switch

  ;; Project management keybindings
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer) ;; Consult project buffer
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project) ;; Switch project
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file) ;; Find file in project
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp) ;; Find regexp in project
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Orgmode keybindings
  (evil-define-key 'normal 'global (kbd "<leader> o a") 'org-agenda) ;; Org agenda
  (evil-define-key 'normal 'global (kbd "<leader> o c") 'org-capture) ;; Org capture

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

  ;; Eglot commands keybindings
  (evil-define-key 'normal eglot-mode-map
    (kbd "<leader> c a") 'eglot-code-actions            ;; Execute code actions
    (kbd "<leader> r n") 'eglot-rename                  ;; Rename symbol
    (kbd "gI") 'eglot-find-implementation               ;; Find implementation
    (kbd "<leader> c f") 'apheleia-format-buffer)          ;; Format buffer via eglot

  (evil-define-key 'normal 'global (kbd "K") 'eldoc-box-help-at-point)

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


(use-package evil-collection
  :defer t
  :straight t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))

;;;; Custom - Auto-generated by Emacs customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
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
