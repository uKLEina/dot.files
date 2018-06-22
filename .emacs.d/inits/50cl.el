(use-package slime
  :config
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  (add-hook 'lisp-mode-hook (slime-mode))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (use-package slime-company)
  (slime-setup '(slime-repl slime-fancy slime-company))
  )
