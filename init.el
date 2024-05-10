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
;; *** better defaults
;; ***

;; UTF-8 as default encoding
(prefer-coding-system 'utf-8)
;; (set-language-environment 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-keyboard-coding-system 'utf-8-unix)
;; add this especially on Windows, else python output problem
;; (set-terminal-coding-system 'utf-8-unix)

;; custom font
(add-to-list 'default-frame-alist '(font . "CaskaydiaCove NF"))


;; (OPTIONAL) Shift width for evil-mode users
;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)

;; change compilation window defaults
(setq compilation-window-height 15)
(setq compilation-scroll-output t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                org-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; Hooks to Enable/Disable Tabs
;; Disable line numbers for some modes
(add-hook 'prog-mode-hook 'my/enable-tabs)
(add-hook 'lisp-mode-hook 'my/disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'my/disable-tabs)

;; customize dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)


;; save emacs backups in a different directory
;; (some build-systems build automatically all files with a prefix, and .#something.someending breakes that)
(setq backup-directory-alist '(("." . "~/.emacsbackups")))


;; Unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(setq dired-dwim-target t)

;; do not show documentation in the minibuffer
(setq eldoc-echo-area-use-multiline-p nil)
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
(defun my/disable-tabs ()
  (setq indent-tabs-mode nil))

(defun my/enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))


;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)


;; ***********************************************************************
;; ***
;; *** OS / Machine Specific Config
;; ***

;; os specific
;; (cond
;;  ((or (string= system-type "ms-dos")
;;       (string= system-type "windows-nt"))
;;   (setq shell-file-name "pwsh.exe"))
;;  ((or (eq system-type 'windows-nt)
;;       (not (file-exists-p "/bin/zsh")))
;;   (setq-default shell-file-name "/bin/zsh")
;;   (setq explicit-shell-file-name "/bin/zsh")))

;; machine specific
;; (cond
;;  ((string= (system-name) "WAP5CG1194FFK")
;;   (setq default-directory "C:/Users/moamen.hraden/")))

;; ***********************************************************************
;; ***
;; *** package repositories
;; ***


(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               t))


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


(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

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

(use-package pdf-tools
  :ensure t)


(use-package multiple-cursors
  :ensure t
  :config
  (require 'multiple-cursors)
  (setq mc/always-run-for-all t))

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
        org-todo-keywords '("TODO" "WIP" "|" "DONE" "BLOCKED" "CANCELED")
        org-todo-keyword-faces '(("TODO" . (:foreground "#ff6e6e" :weight bold))
                                 ("WIP" . (:foreground "#ffea73" :weight bold))
                                 ("DONE" . (:foreground "#98971a" :weight bold))
                                 ("CANCELED" . (:foreground "#ebdbb2" :weight bold))
                                 ("BLOCKED" . (:foreground "#ebdbb2" :weight bold)))))

(use-package ox-latex
  :custom
  (org-latex-listings t))

(use-package ox-gfm
  :ensure t
  :after org)

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

;; (use-package org-roam-ui
;;   :ensure t
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

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

(use-package org-noter
  :ensure t)


;; ***********************************************************************
;; ***
;; *** Programming
;; ***

(add-to-list 'major-mode-remap-alist '((csharp-mode . csharp-ts-mode)
                                       (java-mode . java-ts-mode)
                                       (python-mode . python-ts-mode)
                                       (js-mode . js-ts-mode)
                                       (js2-mode . js-ts-mode)
                                       (css-mod . css-ts-mode)
                                       (typescript-mode . typescript-ts-mode)
                                       (js-json-mode . json-ts-mode)
                                       (go-mode . go-ts-mode)
                                       (yaml-mode . yaml-ts-mode)))

(use-package rust-mode
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package dart-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(use-package eglot
  :ensure t
  :after (yasnippet)
  :hook ((go-ts-mode
          rust-mode
          typescript-ts-mode
          powershell-mode
          nxml-mode
          js-ts-mode
          go-ts-mode
          dart-mode
          latex-mode) . eglot-ensure))


(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-popupinfo-delay 0.2)
  (corfu-auto-prefix 2)
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



;; Add extensions
(use-package cape
  :ensure t
  :bind (("C-c p p" . completion-at-point)))


(use-package yasnippet-capf
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))



;; ***********************************************************************
;; ***
;; *** TODO latex config
;; ***


;; ***********************************************************************
;; ***
;; *** gnus (emacs mail and new reader)
;; ***


(use-package gnus
  :ensure t
  :custom
  (user-full-name "Moamen Hraden")
  (user-mail-address "moamenhredeen@gmail.com")
  ;; (message-send-mail-function 'smtpmail-send-it)
  ;; (smtpmail-default-smtp-server "smtp.gmail.com")
  ;; (smtpmail-smtp-service 587)
  ;; (smtpmail-local-domain "hp-work")
  (gnus-use-cache t)
  (gnus-view-pseudo-asynchronously t)
  (gnus-mime-display-multipart-related-as-mixed t)
  (gnus-use-correct-string-widths nil)
  (gnus-inhibit-images nil)
  (smiley-style 'medium)
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods '((nnml "")
                                   (nnimap "gmail"
                                           (nnimap-address "imap.gmail.com")
                                           (nnimap-server-port 993)
                                           (nnimap-stream ssl)
                                           (nnmail-expiry-target  "nnimap+gmail:[Gmail]/Trash")
                                           (nnir-search-engine imap))))
  (gnus-auto-select-first nil)
  (gnus-summary-display-arrow nil)
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))


(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory (file-name-concat (my/home-directory) "git-repos" "main" "elfeed")))

(use-package elfeed-org
  :ensure t
  :custom
  (rmh-elfeed-org-files
   (list
    (file-name-concat
     (my/home-directory)
     "git-repos"
     "main"
     "elfeed.org")))
  :config
  (elfeed-org))


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

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-nerd-commenter
  :after evil
  :ensure t)

(use-package evil-org
  :ensure t
  :after evil
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
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


;; (use-package hydra
;;   :ensure t
;;   :config
;;   (defhydra h/util-functions ()
;;     "utilities"
;;     ("d" (dired (file-name-concat (my/home-directory) "git-repos" "dot-files")) "dot-files" :column "directories")
;;     ("g" (dired (file-name-concat (my/home-directory) "git-repos")) "git-repos" :column "directories")
;;     ("e" (shell-command "explorer.exe .") "explorer" :column "system")
;;     ("s" (shell-command "Start-Process pwsh") "shell" :column "system")
;;     ("m" my/open-main-org-file "open main.org" :column "quick")))


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
   "M-o"    'tab-switch
   "M-n"    'mc/mark-next-like-this
   "M-N"    'mc/mark-all-like-this)

  (general-define-key
   :states 'normal
   ;; add binding for search workspace symbol
    "gi"    'eglot-find-implementation)

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
    "t"       'eshell
    "c"       'evilnc-comment-or-uncomment-lines
    "e"       'dired-jump

    ;; eglot
    "a"     'eglot-code-actions
    "rr"     'eglot-rename
    "rf"     'eglot-format


    ;; search
    "sb"    'consult-line
    "sg"    'consult-ripgrep
    "so"    'consult-outline
    "si"    'consult-imenu
    "sr"    'consult-register
    "sm"    'consult-mode-command
    "sl"    'consult-goto-line
    "sw"    'occur

    ;; project key biding
    "ps"      'consult-projectile-switch-project
    "pf"      'consult-projectile-find-file
    "pb"      'consult-project-buffer
    "pe"      'projectile-dired

    ;; org roam key binding
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
    "p"       'org-set-property-and-value))


;; ***********************************************************************
;; ***
;; *** User UI Customization
;; ***

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#b5b5b5" :background "#242424"))))
 '(cursor ((t (:background "#ffea73"))))
 '(dired-directory ((t (:foreground "#d4c366" :underline t :bold t))))
 '(eshell-ls-directory ((t (:foreground "#d4c366"))))
 '(eshell-ls-executable ((t (:foreground "#61d874"))))
 '(eshell-prompt ((t (:foreground "#d4c366" :bold t))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-default-face ((t (:inherit 'highlight))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(font-lock-bracket-face ((t (:foreground "#d4c366"))))
 '(font-lock-builtin-face ((t (:foreground "#d4c366"))))
 '(font-lock-comment-face ((t (:foreground "#6f6f6f" :italic t))))
 '(font-lock-constant-face ((t (:foreground "#c5c5c5"))))
 '(font-lock-function-name-face ((t (:foreground "#c5c5c5"))))
 '(font-lock-keyword-face ((t (:foreground "#d4c366" :bold t))))
 '(font-lock-string-face ((t (:foreground "#249c64"))))
 '(font-lock-type-face ((t (:foreground "#c5c5c5" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "#c5c5c5"))))
 '(font-lock-warning-face ((t (:foreground "red" :bold t))))
 '(fringe ((t (:background "#000000"))))
 '(hl-line ((t (:background "#1f1f1f"))))
 '(line-number ((t (:foreground "#6f6f6f"))))
 '(line-number-current-line ((t (:background "#1f1f1f" :foreground "#ffea73"))))
 '(minibuffer-prompt ((t (:foreground "#bfbfbf" :bold t))))
 '(mode-line ((t (:background "#1a1a1a" :foreground "#a1a1a1" :height 1.1))))
 '(mode-line-emphasis ((t (:foreground "red"))))
 '(org-checkbox ((t (:foreground "#d4c366" :bold t))))
 '(org-checkbox-statistics-todo ((t (:foreground "#d4c366" :bold t))))
 '(org-document-title ((t (:foreground "#d4c366" :bold t :height 1.8))))
 '(org-headline-done ((t (:foreground "#878787" :italic t))))
 '(org-level-1 ((t (:foreground "#c5c5c5" :bold t :height 1.5))))
 '(org-level-2 ((t (:foreground "#61d874" :bold t :height 1.3))))
 '(org-level-3 ((t (:foreground "pink" :bold t :height 1.1))))
 '(region ((t (:background "#515151"))))
 '(secondary-selection ((t (:background "#515151"))))
 '(show-paren-match ((t (:foreground "#d4c366" :underline t :bold t))))
 '(vertico-current ((t (:underline "#d2c57d")))))


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
 '(package-selected-packages
   '(flutter multiple-cursors markdown-mode markdown-ts-mode yasnippet-snippets yasnippet-capf which-key vertico undo-tree solarized-theme smartparens rust-mode restclient pdf-tools ox-gfm org-roam org-noter org-journal orderless nov marginalia magit hydra gruvbox-theme git-gutter general evil-surround evil-org evil-nerd-commenter evil-multiedit evil-goggles evil-collection embark-consult elfeed-org docker dired-subtree dart-mode corfu consult-projectile cape)))
