(use-package paredit
  :defer t
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-mode-hook 'enable-paredit-mode)
  )
