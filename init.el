;; ***********************************************************************
;; ***
;; *** My Personal Emacs Configuration
;; ***
;;
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝



;; ***********************************************************************
;; ***
;; *** Utility Functions
;; ***

(defun my/insert-date ()
    "Insert a date at point using `org-read-date' with its optional argument
    of TO-TIME so that the user can customize the date format more easily."
    (interactive)
    (require 'org)
    (let ((time (org-read-date nil 'to-time nil "Date:  ")))
        (insert (format-time-string "%Y-%m-%d" time))))

(defun my/smart-find-file ()
        (interactive)
        (if (projectile-project-p)
            (consult-projectile-find-file)
        (ido-find-file)))

(defun my/smart-find-buffer ()
    (interactive)
    (if (projectile-project-p)
        (consult-project-buffer)
        (consult-buffer)))

    ;; better renaming for new shells
    (defvar shell-index 0)
    (defun my/shell ()
    (interactive)
    (let (shell-name)
        (setq shell-name (read-string "enter shell name (default: <number>-shell): "))
        (if (zerop (length (string-trim shell-name)))
            (progn
            (shell (concat (number-to-string shell-index) "-shell"))
            (setq shell-index (+ shell-index 1)))
        (shell shell-name))))


(defun my/window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
        (let ((func (if (window-full-height-p)
                        #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
            (other-window 1)
            (switch-to-buffer (other-buffer))))))

;; (defun my/open-buffer-in-horizontal-split ()
;;     (interactive)
;;   (split-window-below)
;;   (evil-window-down)
;;   (consult-buffer))

(defun my/home-directory ()
    "os independent home directory.
        this function return home environment variable on linux
        and USERPROFILE environment variable on windows."
  (interactive)
  (if (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
      (getenv "USERPROFILE")
    (getenv "HOME")))

(defun my/open-main-org-file ()
  "shotcut for opening my main org file (second brain)"
  (interactive)
  (find-file (file-name-concat (my/home-directory) "git-repos" "main" "main.org")))


;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))


;; TODO: define function to sync my notes
;; (stage, commit and push changes to github from any file with global keybinding)
;; (defun my/sync-notes ()
;;   (interactive)
;;   (find-file (file-name-concat (my/home-directory) "git-repos" "main" "main.org"))



;; ***********************************************************************
;; ***
;; *** better defaults
;; ***


;; disable warning
(setq warning-minimum-level :emergency)

;; custom font
(add-to-list 'default-frame-alist '(font . "Cascadia Code"))

;; customize cursor line
;; (set-face-attribute 'cursor nil :background "red")


;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Default shell in term
(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)


;; configure file limit
(setq gc-cons-thresold 50000000)
(setq large-file-warning-thresold 100000000)


;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; Speed up startup
(setq auto-mode-case-fold nil)

(setq ring-bell-function 'ignore)

;; always select the newly opend buffer
(setq help-window-select t)

; START TABS CONFIG
;; Create a variable for our preferred tab width
(setq custom-tab-width 4)

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)


;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; (OPTIONAL) Shift width for evil-mode users
;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)

;; WARNING: This will change your life
;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;; Enable whitespace mode everywhere
(global-whitespace-mode)


;; line wrapping
(toggle-truncate-lines 1)

;; change compilation window defaults
(setq compilation-window-height 15)
(setq compilation-scroll-output t)


;; uesr interface centric
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                org-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; remove window fringes
(fringe-mode 0)

(setq initial-scratch-message "
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
")

(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme 'spacemacs-dark t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-height 30)
  (doom-modeline-hud t)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-name t)
  (doom-modeline-env-version t))


;; ***********************************************************************
;; ***
;; *** OS Specific Config
;; ***

;; windows specific config
(when
    (or (string= system-type "ms-dos") (string= system-type "windows-nt"))
    (setq shell-file-name "C:/Users/moamen.hraden/.dotnet/tools/pwsh.exe"))




;; ***********************************************************************
;; ***
;; *** Machine Specific Config
;; ***

;; TODO: do i need this ???
;; hp work pc config
(when
    (string= (system-name) "WAP5CG1194FFK")
    (setq default-directory "C:/Users/moamen.hraden/"))



;; ***********************************************************************
;; ***
;; *** package repositories
;; ***

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)



(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(eval-when-compile
  (require 'use-package))




;; ***********************************************************************
;; ***
;; *** Utility Packages
;; ***

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))


(use-package undo-tree
  :ensure t)


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1))


;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))



;; Example configuration for Consult
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; use the orderless completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-projectile
  :ensure t)


(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  (setq evil-goggles-duration 0.500)
  (custom-set-faces
    '(evil-goggles-default-face ((t (:inherit 'highlight))))))


(use-package restclient
  :ensure t)


(use-package docker
  :ensure t)

(use-package treemacs
  :ensure t)


;; ***********************************************************************
;; ***
;; *** Git configuration
;; ***

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (custom-set-variables
   '(git-gutter:modified-sign " ")
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " "))
  (set-face-background 'git-gutter:modified "#83a598") ;; background color
  (set-face-background 'git-gutter:added "#b8bb26")
  (set-face-background 'git-gutter:deleted "#fb4934")
  (global-git-gutter-mode 1))




;; ***********************************************************************
;; ***
;; *** Org mode
;; ***

(use-package org
  :config
  (add-to-list 'org-agenda-files (file-name-concat (my/home-directory) "git-repos" "main" "main.org"))
  (setq org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t;; do not put two spaces on the left
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-hide-block-startup t
        org-ellipsis " ─╮"
        org-todo-keywords '("TODO" "NEXT" "INPROGRESS" "|" "DONE" "BLOCKED")
        org-todo-keyword-faces '(("TODO" . (:foreground "#ff6e6e" :weight bold :box (:line-width 1)))
                                 ("NEXT" . (:foreground "#cc241d" :weight bold :box (:line-width 1)))
                                 ("INPROGRESS" . (:foreground "#d65d0e" :weight bold :box (:line-width 1)))
                                 ("DONE" . (:foreground "#98971a" :weight bold :box (:line-width 1) ))
                                 ("BLOCKED" . (:foreground "#ebdbb2" :weight bold :box (:line-width 1) )))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)))

(use-package ox-latex
  :custom
  (org-latex-listings t))

(use-package ox-gfm
  :ensure t
  :after org)

;; (setq org-hide-leading-stars t)
;; (setq org-src-fontify-natively t)
;; (global-prettify-symbols-mode t))


(use-package ox-reveal
  :ensure t
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-reveal-mathjax t))

(use-package org-roam
  :ensure t
  :custom
  ;; make sure that the directory exists
  (org-roam-directory (file-name-concat (expand-file-name (my/home-directory)) "git-repos" "main" "org-roam"))
  ;; define org roam templates
  (org-roam-capture-templates '(
    ("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
    ("b" "Book" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: book\n")
      :unnarrowed t)
    ("p" "Project" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: project\n")
      :unnarrowed t)
    ))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :custom
    (org-journal-dir (file-name-concat (expand-file-name (my/home-directory)) "git-repos" "main" "journal"))
    (org-journal-agenda-integration t)
    (org-journal-file-format "%Y-%m.org")
    (org-journal-file-type 'monthly)
    :config
    ;; TODO: do i need this ??
    (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
    (add-to-list 'org-agenda-files org-journal-dir))

    (use-package org-tree-slide
    :ensure t
    :custom
    (org-tree-slide-heading-emphasis t)
    (org-tree-slide-breadcrumbs "❯")
    (org-tree-slide-indicator '(:next "   Next ❯" :previous "❮ Previous" :content "❮  CONTENT  ❯")))



;; ***********************************************************************
;; ***
;; *** Programming
;; ***


(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)))


(use-package yasnippet-snippets
  :ensure t)

(use-package eglot
  :ensure t
  :after (yasnippet)
  :hook ((prog-mode . eglot-ensure)))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-popupinfo-delay 1)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))


(defun corfu-move-to-minibuffer ()
  (interactive)
  (when completion-in-region--data
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data))))
(keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
(add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)



;; ***********************************************************************
;; ***
;; *** TODO latex config
;; ***

;;(use-package auctex)



;; ***********************************************************************
;; ***
;; *** Key Mapping
;; ***

(use-package evil
    :ensure t
    :init
    (setq
        evil-want-integration t
        evil-want-keybinding nil)
    :config
    (evil-mode 1))


(use-package evil-collection
    :ensure t
    :after evil
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init))

(use-package evil-surround
    :ensure t
    :after evil
    :config
    (global-evil-surround-mode 1))

(use-package evil-mc
    :ensure t
    :after evil
    :config
    (global-evil-mc-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :ensure t)

(use-package evil-org
    :ensure t
    :after evil
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
                (lambda () (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; convert escape to super escape :)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; keybinding helper
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-idle-delay 1.0)
  (setq which-key-separator " → " )
  (setq which-key-prefix-prefix "+" ))


;; cleaner way for defining keymap
(use-package general
  :ensure t
  :after evil
  :config

  (general-define-key
   ;; navigation
   "M-k"    'evil-window-up
   "M-j"    'evil-window-down
   "M-h"    'evil-window-left
   "M-l"    'evil-window-right
   "M-s"    'split-window-below
   "M-v"    'split-window-right
   "M-q"    'evil-quit
   "M-o"    'tab-switch)

  (general-create-definer my-leader-def
    :prefix "SPC")

  ;; Global Keybindings
  (my-leader-def
    :states   '(normal visual)
    :keymaps  'override

    "SPC"   'recompile

    ;; most used commands
    "w"       'save-buffer
    "f"       'find-file        ;;'my/smart-find-file
    "b"       'consult-buffer   ;;'my/smart-find-buffer
    "q"       'delete-window
    "k"       'magit-status
    "x"       'execute-extended-command
    "h"       'consult-apropos
    "d"       'docker
    "t"       'my/shell
    "c"       'evilnc-comment-or-uncomment-lines
    "e"         'treemacs

    ;; eglot
    ;; "aa"     'eglot-code-actions
    ;; "ar"     'eglot-rename
    ;; "af"     'eglot-format


    ;; search
    "sb"    'consult-line
    "sg"    'consult-ripgrep
    "so"    'consult-outline
    "si"    'consult-imenu
    "sr"    'consult-register
    "sm"    'consult-mode-command
    "sl"    'consult-goto-line
    "st"    'consult-theme
    "sw"    'occur

    ;; utility binding functions
    "ud"      'my/insert-date

    ;; project key biding
    "ps"      'consult-projectile-switch-project
    "pf"      'consult-projectile-find-file
    "pb"      'consult-project-buffer

    ;; org roam key binding
    "oi"        'my/open-main-org-file
    "oa"        'org-agenda
    "oc"        'org-capture
    "of"        'org-roam-node-find
    "ob"        'org-roam-buffer
    "ot"        'org-roam-buffer-toggle

    ;; org journal key binding
    "jn"      'org-journal-next-entry
    "jp"      'org-journal-previous-entry
    "ji"      'org-journal-new-entry
    "js"      'org-journal-search)

  ;; Org Mode Keybindings
  (general-define-key
   :keymaps  '(org-tree-slide-mode-map override)
   :states   '(normal insert)
   "<right>"     'org-tree-slide-move-next-tree
   "<left>"     'org-tree-slide-move-previous-tree)

  ;; Org Mode evil Keybindings
  ;; org mode local leader
  (general-create-definer my-local-leader-def
    :prefix   "SPC .")

  (my-local-leader-def
    :states   'normal
    :keymaps  'org-mode-map
    "h"      'consult-org-heading
    "l"       'org-insert-link
    "t"       'org-set-tags-command
    "p"       'org-set-property-and-value
    "e"       'org-tree-slide-mode))



;; ***********************************************************************
;; ***
;; *** User UI Customization
;; ***

(custom-theme-set-faces
 'user
 '(org-document-title ((t (:inherit bold :foreground "#bc6ec5" :underline t :height 1.8))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.5))))
 '(org-headline-done ((t (:foreground "#878787")))))

;; ***********************************************************************
;; ***
;; *** Auto Generated
;; ***

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(org-fontify-done-headline t)
 '(package-selected-packages
   '(yasnippet-snippets which-key web-mode vscode-dark-plus-theme vertico undo-tree spacemacs-theme spaceline space-theming smartparens restclient ox-reveal ox-gfm org-tree-slide org-roam-ui org-journal org-bullets orderless nano-theme nano-modeline monokai-theme moe-theme marginalia magit highlight-indentation go-mode git-gutter general evil-surround evil-org evil-nerd-commenter evil-mc evil-goggles evil-collection embark-consult eat doom-themes doom-modeline docker corfu consult-projectile company atom-one-dark-theme ample-theme all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-default-face ((t (:inherit 'highlight))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(org-headline-done ((t (:foreground "#878787"))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.5)))))
