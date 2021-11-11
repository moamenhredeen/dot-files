;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.




;; import configs from config.org file
(require 'org)
(org-babel-load-file (expand-file-name "/home/moamenhredeen/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck yasnippet-snippets use-package undo-tree restclient-test org-bullets gruvbox-theme evil doom-modeline counsel company-auctex auto-complete))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#C6DE41" :foreground "black"))))
 '(doom-modeline-bar ((t (:inherit highlight :background "black"))))
 '(hl-line ((t (:background "#1D1D00"))))
 '(line-number ((t (:inherit default :background "#3c3836" :foreground "#C6DE41"))))
 '(line-number-current-line ((t (:inherit default :background "#1D1D00" :foreground "#C6DE41" :inverse-video t :weight ultra-bold))))
 '(org-checkbox ((t (:foreground "#fe8019" :weight extra-bold))))
 '(org-hide ((t (:foreground "#282828"))))
 '(org-level-1 ((t (:foreground "#fb4934" :weight extra-bold :height 1.4))))
 '(org-level-2 ((t (:foreground "#b8bb26" :weight extra-bold :height 1.2))))
 '(org-level-3 ((t (:foreground "#fabd2f" :weight bold))))
 '(org-level-4 ((t (:foreground "#d3869b"))))
 '(org-level-5 ((t (:foreground "#83a598"))))
 '(org-tag ((t (:foreground "#7CBD1E" :weight bold))))
 '(region ((t (:background "#44000D" :weight bold)))))
