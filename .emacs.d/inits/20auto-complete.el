(use-package auto-complete
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
  :config
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (custom-set-variables '(ac-auto-start 2))
  (custom-set-variables '(ac-delay 0.05))
  (custom-set-variables '(ac-use-menu-map t))
  (ac-set-trigger-key "C-M-i")
  (bind-keys :map ac-menu-map
             ("C-n" . ac-next)
             ("C-p" .ac-previous)))
