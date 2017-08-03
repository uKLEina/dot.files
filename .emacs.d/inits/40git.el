(use-package git-commit
  :mode (("COMMIT_EDITMSG" . git-commit-mode))
  :init (evil-set-initial-state 'git-commit-mode 'normal)
  )
