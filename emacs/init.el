;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.




;; import configs from config.org file
(require 'org)
(org-babel-load-file (expand-file-name "/home/moamenhraden/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default))
 '(package-selected-packages
   '(helm-core helm svg-lib gnuplot gnuplot-mode impatient-mode markdown-preview-mode markdown-mode org-present presentation org-preview-html latex-math-preview latex-preview-pane company-auctex auto-complete-auctex auctex yatex company-tabnine ewal-spacemacs-themes zenburn-theme yasnippet-snippets solarized-theme rjsx-mode request req-package prettier org-pomodoro org-bullets nyan-mode minimap magit gruvbox-theme flyspell-popup flycheck-irony evil-leader evil-escape doom-themes doom-modeline counsel company-irony clojure-mode auto-complete)))
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
 '(org-checkbox ((t (:background "#565D47" :foreground "#81B214" :weight extra-bold))))
 '(org-hide ((t (:foreground "#282828"))))
 '(org-level-1 ((t (:foreground "#4ECCA3" :weight extra-bold :height 1.2))))
 '(org-level-8 ((t (:foreground "#fe8019"))))
 '(org-tag ((t (:foreground "#7CBD1E" :weight bold))))
 '(region ((t (:background "#44000D" :weight bold)))))
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
