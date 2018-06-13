(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-repl slime-fancy))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . slime-mode))
  )
