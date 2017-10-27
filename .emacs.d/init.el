(setq load-prefer-newer t)
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq package-user-dir "~/.emacs.d/elisp")
(package-initialize)

(require 'use-package)

(use-package init-loader
  :config
  (init-loader-load "~/.emacs.d/inits"))
