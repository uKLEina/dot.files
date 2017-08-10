(setq load-prefer-newer t)

(setq package-user-dir "~/.emacs.d/elisp")
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 2)
 '(ac-delay 0.01)
 '(ac-use-menu-map t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(anzu-use-migemo t)
 '(company-idle-delay nil)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(helm-smex-show-bindings t)
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(jedi:tooltip-method nil)
 '(package-selected-packages
   (quote
    (ac-octave helm-projectile projectile ac-dabbrev switch-buffer-functions google-this smartrep all-the-icons-dired all-the-icons spaceline spaceline-all-the-icons powershell company-c-headers helm-themes sudo-edit flyspell-correct flyspell-correct-helm ac-ispell dired-quick-sort php-mode js2-mode use-package zlc zenburn-theme yaml-mode virtualenvwrapper vimrc-mode tuareg sql-indent smartparens ripgrep resize-window region-bindings-mode quickrun python-mode py-autopep8 popwin plantuml-mode org open-junk-file neotree migemo meghanada markdown-mode jedi irony-eldoc init-loader imenu-list highlight-symbol helm-swoop helm-smex helm-gtags helm-company helm-c-yasnippet helm-ag google-translate git-commit geben flycheck-pyflakes flycheck-pos-tip flycheck-irony expand-region exec-path-from-shell evil-paredit ddskk company-quickhelp company-irony cmake-mode cmake-ide clang-format backward-forward auto-save-buffers-enhanced auto-compile anzu ac-php ac-js2 ac-helm)))
 '(rtags-display-result-backend "Helm")
 '(rtags-popup-results-buffer t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'use-package)

(use-package init-loader
  :defer t
  :init (init-loader-load "~/.emacs.d/inits"))
