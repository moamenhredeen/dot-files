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

;; configure file limit
(setq gc-cons-thresold 50000000)
(setq large-file-warning-thresold 100000000)

;; configure encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


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

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

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
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (setq whitespace-display-mappings
;;       '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;; Enable whitespace mode everywhere
;;(global-whitespace-mode)

;; remove window title bar
;; (setq default-frame-alist '((undecorated . t)))
;; add thin border to be able to resize the window
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(drag-with-header-line . 1))
;; (add-to-list 'default-frame-alist '(drag-with-mode-line . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

;; maximaize window by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; line wrapping
(toggle-truncate-lines 1)

;; change compilation window defaults
(setq compilation-window-height 15)
(setq compilation-scroll-output t)





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
;; *** Customize user interface
;; ***

(setq initial-scratch-message "
 ██╗  ██╗██╗    ███╗   ███╗ ██████╗  █████╗ ███╗   ███╗███████╗███╗   ██╗
 ██║  ██║██║    ████╗ ████║██╔═══██╗██╔══██╗████╗ ████║██╔════╝████╗  ██║
 ███████║██║    ██╔████╔██║██║   ██║███████║██╔████╔██║█████╗  ██╔██╗ ██║
 ██╔══██║██║    ██║╚██╔╝██║██║   ██║██╔══██║██║╚██╔╝██║██╔══╝  ██║╚██╗██║
 ██║  ██║██║    ██║ ╚═╝ ██║╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗██║ ╚████║
 ╚═╝  ╚═╝╚═╝    ╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝
")


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

;; set theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))



(use-package all-the-icons
  :ensure t)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30))


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



(use-package highlight-indentation
  :ensure t
  :config
  (highlight-indentation-current-column-mode 1))

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
  (add-to-list 'org-agenda-files (file-name-concat (getenv "HOME") "git-repos" "main"))
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t;; do not put two spaces on the left
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t))

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

(use-package org-bullets
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook org-mode
  :config
  (setq org-bullets-face-name (quote org-bullet-face))
  ;;  "➜" ❱ ■   ≡ ∷ ∞ ∬  ━┓ ┉  ┅ ┋┊┋  ╏╏ ║ ═  │ ━ ╭ ─╮ ╱ ⤵
  ;; ▆ ▇ ██  ░░  ▒▒
  ;; ① ② ③ ④ ⑤
  ;; Ⓐ Ⓑ Ⓒ Ⓓ Ⓔ
  ;; ⓐ ⓑ ⓒ ⓓ ⓔ
  ;; ⓵ ⓶ ⓷ ⓸ ⓹
  ;; ✤ ✣ ✻ ✽ ✼ ✹ ✺ ❆ ❅ ❖
  ;; ✒ ✐ ✔ ✓ ✘  ❱ ❯
  ;; ⣿ ⠿  ▤
  (setq org-bullets-bullet-list '("≡" "❯" "➜" "✻" "✺"))
  (setq org-ellipsis " ─╮")
  ;; (setq org-todo-keywords '((sequence "TODO" "NEXT" "INPROGRESS" "|" "DONE" "BLOCKED")))
  ;; (setq org-todo-keyword-faces
	;;       '(("TODO" . (:foreground "#cc241d" :weight bold :inverse-video t))
	;;         ("NEXT" . (:foreground "#cc241d" :weight bold :inverse-video t))
	;;         ("INPROGRESS" . (:foreground "#d65d0e" :weight bold :inverse-video t))
	;;         ("DONE" . (:foreground "#98971a" :weight bold :inverse-video t))
	;;         ("BLOCKED" . (:foreground "#ebdbb2" :weight bold :inverse-video t))))

  (setq org-link-abbrev-alist
        '(("ggle" . "http://www.google.com/search?q=%s")
          ("gmap" . "http://maps.google.com/maps?q=%s")
          ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (global-prettify-symbols-mode t))



(use-package ox-reveal
  :ensure t
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-reveal-mathjax t))

(use-package org-roam
  :ensure t
  :custom
  ;; make sure that the directory exists
  (org-roam-directory (file-name-concat (expand-file-name (getenv "HOME"))"git-repos" "main" "org-roam"))
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
  (org-journal-dir (file-name-concat (expand-file-name (getenv "USERPROFILE"))"git-repos" "main" "journal"))
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

(use-package pascal-mode
  :mode ("\\.iss\\'" . pascal-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (setq compile-command "go run main.go"))

(use-package lua-mode
  :ensure t
  :defer t)

(use-package eglot
  :ensure t
  :defer t
  :hook ((prog-mode . eglot-ensure)))

(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode))
  :config
  (setq company-backends '((company-capf :with company-yasnippet :separate)
                           (company-yasnippet :separate)))
  (define-key company-active-map
              (kbd "TAB")
              #'company-complete-common-or-cycle)
  (define-key company-active-map
              (kbd "<backtab>")
              (lambda ()
                (interactive)
                (company-complete-common-or-cycle -1))))


;; load opascal-mode automatically for these file extensions
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(pas\\|iss\\)\\'" . pascal-mode))

;; ;; configure pascal lsp
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(pascal-mode . ("fools" "--stdio"))))




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
  (shell (concat (number-to-string shell-index) "-shell"))
  (setq shell-index (+ shell-index 1)))

;; ***********************************************************************
;; ***
;; *** Key Mapping
;; ***

(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
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
    :config
    (global-evil-surround-mode 1))

(use-package evil-mc
    :ensure t
    :config
    (global-evil-mc-mode 1))

(use-package evil-org
    :ensure t
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
  :config
  (general-create-definer my-leader-def
    :prefix "SPC")

  ;; Global Keybindings
  (my-leader-def
    :states   'normal
    :keymaps  'override

    "SPC"   'recompile

    ;; most used commands
    "w"       'save-buffer
    "f"       'my/smart-find-file
    "b"       'my/smart-find-buffer
    "q"       'delete-window
    "k"       'magit-status
    "x"       'execute-extended-command
    "h"       'consult-apropos
    "d"       'docker
    "t"       'my/shell

    ;; search
    "sb"    'consult-line
    "sp"    'consult-ripgrep
    "so"    'consult-outline
    "si"    'consult-imenu
    "sr"    'consult-register
    "sm"    'consult-mode-command
    "sl"    'consult-goto-line
    "st"    'consult-theme


    ;; utility functions binding
    "ud"      'my/insert-date

    ;; org key binding

    ;; general buffe
    "gf"      'find-file
    "gb"      'consult-buffer

    ;; project key biding
    "ps"      'consult-projectile-switch-project

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
    :prefix   "SPC e")

  (my-local-leader-def
    :states   'normal
    :keymaps  'org-mode-map
    "h"      'consult-org-heading
    "l"       'org-insert-link
    "t"       'org-set-tags-command
    "p"       'org-set-property-and-value
    "e"       'org-tree-slide-mode))



(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;;(setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (global-set-key (kbd "M-o") 'ace-window))



;; ***********************************************************************
;; ***
;; *** Auto Generated
;; ***

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" "79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" default))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(git-gutter:visual-line t)
 '(git-gutter:window-margin 3)
 '(git-gutter:window-width 1)
 '(org-agenda-files '("c:/Users/moame/git-repos/main/plan.org"))
 '(package-selected-packages
   '(zig-mode wttrin cargo beacon lua-mode restclient omnisharp evil-goggles highlight-indentation ace-window rg go-mode iss-mode lsp-mode pyvenv pyvenv-mode pyenv-mode jupyter angular-mode scss-mode vterm evil-mc consult-projectile docker wgrep git-gutter nyan-mode minimap darcula-theme ox-gfm ox-beamer ox-md csharp-mode tree-sitter-indent yasnippet company eglot elgot rust-mode tree-sitter org-journal org-roam-ui embark-consult embark org-roam magit which-key general marginalia orderless evil-escape all-the-icons vertico-directory vertico evil-collection evil-org evil-surround evil ox-reveal doom-modeline gruvbox-theme use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 100 :width normal :family "Cascadia Code" :foreground "#B8B8B8"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-default-face ((t (:inherit 'highlight))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(whitespace-tab ((t (:foreground "#636363")))))
