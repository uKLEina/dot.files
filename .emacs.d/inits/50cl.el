(use-package slime
  :config
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . slime-mode))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (use-package slime-company)
  (slime-setup '(slime-repl slime-fancy slime-company))
  (add-hook 'slime-mode-hook 'enable-paredit-mode)
  )
