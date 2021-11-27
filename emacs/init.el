;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.




;; import configs from config.org file
(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "a44bca3ed952cb0fd2d73ff3684bda48304565d3eb9e8b789c6cca5c1d9254d1" "79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" default))
 '(lsp-clients-texlab-executable "/home/moamen/.cargo/bin/texlab")
 '(package-selected-packages
   '(lsp-java which-key dap-mode lsp-ivy lsp-mode all-the-icons-ivy solarized-theme monokai-pro-theme leuven-theme lueven dired-filter dired-subtree company-reftex ox-reveal elpy apheleia all-the-icons-dired projectile evil-mc dictionary wttrin highlight-indent-guides undo-fu multiple-cursors rainbow-delimiters git-gutter evil-magit htmlize darcula-theme flycheck yasnippet-snippets use-package undo-tree restclient-test org-bullets gruvbox-theme evil doom-modeline counsel company-auctex auto-complete))
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
 '(org-hide ((t (:foreground "#282828"))))
 '(org-level-1 ((t (:weight extra-bold :height 1.4))))
 '(org-level-2 ((t (:weight extra-bold :height 1.2))))
 '(region ((t (:background "#44000D" :weight bold)))))
