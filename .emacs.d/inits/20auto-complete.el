(use-package auto-complete
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
  :config
  (ac-config-default)
  (setq ac-use-menu-map t)
  (bind-key "C-M-i" ac-mode-map)
  (bind-keys :map ac-menu-map
             ("C-n" . ac-next)
             ("C-p" .ac-previous)))
