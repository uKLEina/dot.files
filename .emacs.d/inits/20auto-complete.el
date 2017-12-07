(use-package auto-complete-config
  :defer t
  :commands (ac-emacs-lisp-mode-setup)
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  :config
  (use-package ac-dabbrev)
  (setq-default ac-sources '(ac-source-filename ac-source-dabbrev ac-source-words-in-same-mode-buffers))
  (custom-set-variables
   '(ac-auto-start 2)
   '(ac-delay 0.01)
   '(ac-use-menu-map t)))
