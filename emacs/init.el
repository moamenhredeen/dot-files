;; import configs from config.org file
(require 'org)
(org-babel-load-file (expand-file-name "~/.config/emacs/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("735561d82728e28f275802fc875c3a2caf14d06f434604a7516c59d49120b163" default))
 '(package-selected-packages
   '(dap-mode yasnippet company rust-mode eglot org-bullets gruvbox-theme orderless typescript-mode lsp-mode vertico use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
