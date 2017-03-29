
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'init-loader "~/.emacs.d/elisp/init-loader/init-loader")
(init-loader-load "~/.emacs.d/inits")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 100)
 '(anzu-use-migemo t)
 '(company-idle-delay nil)
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(package-selected-packages
   (quote
    (company-quickhelp meghanada helm-company company-irony ac-helm ac-js2 js2-mode ac-php flycheck-pyflakes yaml-mode git-commit tuareg backward-forward zenburn-theme python-mode point-undo markdown-mode smartrep google-translate evil-paredit paredit vimrc-mode flycheck-irony cmake-mode irony region-bindings-mode expand-region helm-c-yasnippet smartparens py-autopep8 jedi ctags-update exec-path-from-shell helm-swoop helm-ag ddskk ripgrep helm-smex flycheck evil anzu resize-window quickrun highlight-symbol migemo popwin auto-save-buffers-enhanced auto-compile init-loader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
