;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change Emacs Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq custom-tab-width 2)

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
;;(setq whitespace-style '(face tabs tab-mark trailing))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;;(global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

;; show line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and initialise package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install use-package package if not installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(eval-when-compile
  (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-scratch-message
      "
;; ██╗  ██╗██╗    ███╗   ███╗ ██████╗  █████╗ ███╗   ███╗███████╗███╗   ██╗
;; ██║  ██║██║    ████╗ ████║██╔═══██╗██╔══██╗████╗ ████║██╔════╝████╗  ██║
;; ███████║██║    ██╔████╔██║██║   ██║███████║██╔████╔██║█████╗  ██╔██╗ ██║
;; ██╔══██║██║    ██║╚██╔╝██║██║   ██║██╔══██║██║╚██╔╝██║██╔══╝  ██║╚██╗██║
;; ██║  ██║██║    ██║ ╚═╝ ██║╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗██║ ╚████║
;; ╚═╝  ╚═╝╚═╝    ╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝")


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
		  treemacs-mode-hook
		  eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; set theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))


(use-package all-the-icons
  :ensure t)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
  )

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
  :bind(
  ;; Replace bindings. Lazily loaded due by `use-package'.
  
         ("C-c m" . consult-mode-command)
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;;:hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  ;;:init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  ;;(setq register-preview-delay 0.5
  ;;      register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;;(advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  ;;(setq xref-show-xrefs-function #'consult-xref
  ;;      xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  ;; :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  ;; :preview-key (kbd "M-.")
  ;;  :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git configuraiton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


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

;; TODO: define better search function
(defun my/consult-ripgrep-current-file ()
    "Call `consult-ripgrep' for the current buffer (a single file)."
    (interactive)
    (let ((consult-project-function (lambda (x) nil))
          (consult-ripgrep-args
           (concat "rg "
                   "--null "
                   "--line-buffered "
                   "--color=never "
                   "--line-number "
                   "--smart-case "
                   "--no-heading "
                   "--max-columns=1000 "
                   "--max-columns-preview "
                   "--with-filename "
                   (shell-quote-argument buffer-file-name))))
      (consult-ripgrep)))

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

    ;; most used commands 
    "w"       'save-buffer
    "f"       'find-file
    "b"       'consult-buffer
    "q"       'delete-window
    ;;"s"       'my/consult-ripgrep-current-file
    "s"       'my/consult-ripgrep-current-file
    "k"       'magit-status
    "x"       'execute-extended-command
    "h"       'consult-apropos

    ;; org key binding
    "a"       'org-agenda
    "c"       'org-capture

    ;; org roam key binding
    "rf"      'org-roam-node-find
    "rb"      'org-roam-buffer
    "rt"      'org-roam-buffer-toggle

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
  (my-leader-def
    :states   'normal
    :keymaps  'org-mode-map
    "gh"      'consult-org-heading
    "y"       'org-store-link
    "p"       'org-insert-link
    "ri"      'org-roam-node-insert
    "rs"      'org-roam-db-sync)

  ;; org mode local leader 
  (general-create-definer my-local-leader-def
    :prefix   "SPC l")

  (my-local-leader-def
    :states     'normal
    :keymaps    '(org-mode-map org-tree-slide-mode-map)
    "s"         'org-tree-slide-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :config
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t;; do not put two spaces on the left
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t)
  (add-to-list 'org-agenda-files (file-name-concat (expand-file-name (getenv "USERPROFILE"))"git-repos" "main")))

(use-package org-bullets
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook org-mode
  :config
  (setq org-bullets-face-name (quote org-bullet-face))
  (setq org-bullets-bullet-list '("▤" "▶" "➜"))
  (setq org-ellipsis " ⤵")
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "INPROGRESS" "|" "DONE" "BLOCKED")))
  (setq org-todo-keyword-faces
	      '(("TODO" . (:foreground "#cc241d" :weight bold :inverse-video t))
	        ("NEXT" . (:foreground "#cc241d" :weight bold :inverse-video t))
	        ("INPROGRESS" . (:foreground "#d65d0e" :weight bold :inverse-video t))
	        ("DONE" . (:foreground "#98971a" :weight bold :inverse-video t))
	        ("BLOCKED" . (:foreground "#ebdbb2" :weight bold :inverse-video t))))

  (setq org-link-abbrev-alist
        '(("ggle" . "http://www.google.com/search?q=%s")
          ("gmap" . "http://maps.google.com/maps?q=%s")
          ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (global-prettify-symbols-mode t))



;;(require 'ox-md)
;;(require 'ox-beamer)
;; (use-package ox-reveal
;;   :ensure t
;;   :custom
;;   (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
;;   (org-reveal-mathjax t))

(use-package org-roam
  :ensure t
  :custom
  ;; make sure that the directory exists
  (org-roam-directory (file-name-concat (expand-file-name (getenv "USERPROFILE"))"git-repos" "main" "org-roam"))
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
  ;; create org-roam directory if it does not exist
  ;; (let ((my-roam-dir (file-name-concat (expand-file-name (getenv "userprofile"))"git-repos" "main" "org-roam")))
	;;  (if (not (file-exists-p my-roam-dir)) (make-directory  my-roam-dir))
  ;; (setq org-roam-directory my-roam-dir))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto generated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("c:/Users/moame/git-repos/main/plan.org"))
 '(package-selected-packages
   '(org-tree-slide epresent org-journal org-roam-ui embark-consult embark org-roam magit which-key general marginalia orderless evil-escape all-the-icons vertico-directory vertico evil-collection evil-org evil-surround evil ox-reveal doom-modeline gruvbox-theme use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 150 :width normal :family "Cascadia Code"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
