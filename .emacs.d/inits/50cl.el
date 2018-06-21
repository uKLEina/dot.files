(use-package slime
  :config
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . slime-mode))
  (add-hook 'slime-mode-hook 'lisp-mode)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (use-package slime-company)
  (slime-setup '(slime-repl slime-fancy slime-company))
  )
