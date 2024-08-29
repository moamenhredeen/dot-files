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


(defun my/home-directory ()
    "os independent home directory.
this function return home environment variable on linux
and USERPROFILE environment variable on windows."
  (interactive)
  (if (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
      (getenv "USERPROFILE")
    (getenv "HOME")))


(defun my/disable-tabs ()
  (setq indent-tabs-mode nil))

(defun my/enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; todo fix error
;; (defun my/add-hooks (hooks function)
;;   "my/add-hooks enable you to attach function to a list of hooks
;; Example:
;;     (my/add-hooks '(first-hook second-hook) some-function)"
;;   (mapc (lambda (hook)
;;           (add-hook hook function))
;;         hooks))

;; (defun my/open-init-file ()
;;   "Open the init file."
;;   (interactive)
;;   (find-file user-init-file))


;; ***********************************************************************
;; ***
;; *** better defaults
;; ***


;; variables
(setq my/notes-directory
	  (file-name-concat (expand-file-name (my/home-directory))  "notes"))

(setq-default evil-shift-width custom-tab-width)
(setq compilation-window-height 15
      compilation-scroll-output t
      dired-dwim-target t
	  tab-bar-show nil
      ;; do not show documentation in the minibuffer
      eldoc-echo-area-use-multiline-p nil
      backup-directory-alist '(("." . "~/.emacsbackups")))


;; modes
(pixel-scroll-precision-mode)

;; functions
(prefer-coding-system 'utf-8)

;; alists
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF"))


;; hooks
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; tabs hooks
(add-hook 'lisp-mode-hook 'my/disable-tabs)
(add-hook 'prog-mode-hook 'my/enable-tabs)
(add-hook 'emacs-lisp-mode-hook 'my/enable-tabs)

;; disable line nubmer hooks
(defun my/disable-line-numbers ()
  (display-line-numbers-mode 0))
(add-hook 'shell-mode-hook 'my/disable-line-numbers)
(add-hook 'org-mode-hook 'my/disable-line-numbers)
(add-hook 'treemacs-mode-hook 'my/disable-line-numbers)
(add-hook 'eshell-mode-hook 'my/disable-line-numbers)
(add-hook 'pdf-view-mode-hook 'my/disable-line-numbers)



;; packagess
(use-package emacs
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        enable-recursive-minibuffers t
        minibuffer-prompt-properties
		'(read-only t cursor-intangible t face minibuffer-prompt)))


;; ***********************************************************************
;; ***
;; *** package repositories
;; ***

;; elpa (default repo)
(package-initialize)

;; melpa
(use-package package
  :config
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))


;; ***********************************************************************
;; ***
;; *** Org mode
;; ***

(use-package org
  :ensure t
  :config
  (setq org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-hide-block-startup t
		org-default-notes-file (file-name-concat my/notes-directory "inbox.org")
		org-agenda-files '("/home/moamen/notes")
        org-ellipsis " ─╮"
        org-todo-keywords '("TODO" "WIP"
							"|"
							"DONE" "BLOCKED" "CANCELED")
        org-todo-keyword-faces '(("TODO" . (:foreground "#ff6e6e" :weight bold))
                                 ("WIP" . (:foreground "#ffea73" :weight bold))
                                 ("DONE" . (:foreground "#98971a" :weight bold))
                                 ("CANCELED" . (:foreground "#ebdbb2" :weight bold))
                                 ("BLOCKED" . (:foreground "#ebdbb2" :weight bold)))))

(use-package ox-latex
  :custom
  (org-latex-listings t))


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-name-concat my/notes-directory "org-roam"))
  (org-roam-capture-templates '(
    ("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
#+STARTUP: fold
#+date: %U
")
      :unnarrowed t)
    ("b" "Book" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
#+STARTUP: fold
#+date: %U
#+filetags: book
")
      :unnarrowed t)
    ("p" "Project" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
#+STARTUP: fold
#+date: %U
#+filetags: project
")
      :unnarrowed t)
    )))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :custom
    (org-journal-dir (file-name-concat my/notes-directory "journal"))
    (org-journal-agenda-integration t)
    (org-journal-file-format "%Y-%m.org")
    (org-journal-file-type 'monthly)
    :config
    ;; TODO: do i need this ??
    (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
    (add-to-list 'org-agenda-files org-journal-dir))


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

;; TODO: is there better alternatives
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


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode))


;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))


(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
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

(use-package restclient
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
;; *** Programming
;; ***

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(use-package typescript
  :ensure t)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-ts-mode . lsp-deferred)
		 (html-mode . lsp-deferred))
  :commands (lsp lsp-deferred))



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


(use-package cape
  :ensure t
  :bind (("C-c p p" . completion-at-point)))

(use-package yasnippet-capf
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))



;; ***********************************************************************
;; ***
;; *** Key Mapping
;; ***

(use-package evil
  :ensure t
  :init
  (setq
   evil-undo-system 'undo-redo
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
  :hook (evil-org . org-mode)
  :after evil
  :after org
  :config
  ;; (add-hook 'org-mode-hook 'evil-org-mode)
  ;; (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; keybinding helper
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode)
;;   (setq which-key-popup-type 'side-window)
;;   (setq which-key-side-window-location 'bottom)
;;   (setq which-key-side-window-max-width 0.33)
;;   (setq which-key-side-window-max-height 0.25)
;;   (setq which-key-idle-delay 1.0)
;;   (setq which-key-separator " → " )
;;   (setq which-key-prefix-prefix "+" ))


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
   :states		'(normal visual)
    "g c"		'evilnc-comment-or-uncomment-lines
    "g i"		'eglot-find-implementation)

  (general-define-key
   :states		'normal
   :keymaps		'emacs-lisp-mode-map
   "K"			'describe-function)

  (general-create-definer my-leader-def
    :prefix		"SPC")

  ;; Global Keybindings
  (my-leader-def
    :states   '(normal visual)
    :keymaps  'override

    ;; most used commands
    "w"       'save-buffer
    "f"       'find-file
    "b"       'consult-buffer
    "q"       'delete-window
    "k"       'magit-status
    "x"       'execute-extended-command
    "h"       'consult-apropos
    "d"       'docker
    "t"       'eshell
    "c"       'my/open-init-file
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
	"oi"		(lambda () (interactive) (find-file (file-name-concat my/notes-directory "inbox.org")))

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


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ***********************************************************************
;; ***
;; *** User UI Customization
;; ***

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:foreground "#b5b5b5" :background "#242424"))))
;;  '(cursor ((t (:background "#ffea73"))))
;;  '(dired-directory ((t (:foreground "#d4c366" :underline t :bold t))))
;;  '(eshell-ls-directory ((t (:foreground "#d4c366"))))
;;  '(eshell-ls-executable ((t (:foreground "#61d874"))))
;;  '(eshell-prompt ((t (:foreground "#d4c366" :bold t))))
;;  '(evil-goggles-change-face ((t (:inherit diff-removed))))
;;  '(evil-goggles-default-face ((t (:inherit 'highlight))))
;;  '(evil-goggles-delete-face ((t (:inherit diff-removed))))
;;  '(evil-goggles-paste-face ((t (:inherit diff-added))))
;;  '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
;;  '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
;;  '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
;;  '(evil-goggles-yank-face ((t (:inherit diff-changed))))
;;  '(font-lock-bracket-face ((t (:foreground "#d4c366"))))
;;  '(font-lock-builtin-face ((t (:foreground "#d4c366"))))
;;  '(font-lock-comment-face ((t (:foreground "#6f6f6f" :italic t))))
;;  '(font-lock-constant-face ((t (:foreground "#c5c5c5"))))
;;  '(font-lock-function-name-face ((t (:foreground "#c5c5c5"))))
;;  '(font-lock-keyword-face ((t (:foreground "#d4c366" :bold t))))
;;  '(font-lock-string-face ((t (:foreground "#249c64"))))
;;  '(font-lock-type-face ((t (:foreground "#c5c5c5" :bold t))))
;;  '(font-lock-variable-name-face ((t (:foreground "#c5c5c5"))))
;;  '(font-lock-warning-face ((t (:foreground "red" :bold t))))
;;  '(fringe ((t (:background "#000000"))))
;;  '(hl-line ((t (:background "#1f1f1f"))))
;;  '(line-number ((t (:foreground "#6f6f6f"))))
;;  '(line-number-current-line ((t (:background "#1f1f1f" :foreground "#ffea73"))))
;;  '(minibuffer-prompt ((t (:foreground "#bfbfbf" :bold t))))
;;  '(mode-line ((t (:background "#1a1a1a" :foreground "#a1a1a1" :height 1.1))))
;;  '(mode-line-emphasis ((t (:foreground "red"))))
;;  '(org-checkbox ((t (:foreground "#d4c366" :bold t))))
;;  '(org-checkbox-statistics-todo ((t (:foreground "#d4c366" :bold t))))
;;  '(org-document-title ((t (:foreground "#d4c366" :bold t :height 1.8))))
;;  '(org-headline-done ((t (:foreground "#878787" :italic t))))
;;  '(org-level-1 ((t (:foreground "#c5c5c5" :bold t :height 1.5))))
;;  '(org-level-2 ((t (:foreground "#61d874" :bold t :height 1.3))))
;;  '(org-level-3 ((t (:foreground "pink" :bold t :height 1.1))))
;;  '(region ((t (:background "#515151"))))
;;  '(secondary-selection ((t (:background "#515151"))))
;;  '(show-paren-match ((t (:foreground "#d4c366" :underline t :bold t))))
;;  '(vertico-current ((t (:underline "#d2c57d")))))

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
   '(lsp-mode typescript yasnippet-snippets yasnippet-capf vertico smartparens restclient org-roam org-journal orderless multiple-cursors marginalia magit git-gutter general evil-surround evil-org evil-nerd-commenter evil-multiedit evil-collection embark-consult dired-subtree corfu consult-projectile cape)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
