(use-package evil-mc
  :defer t
  :commands (evil-mc-make-all-cursors)
  :bind (("C-l m u l" . evil-mc-make-all-cursors))
  :config
  (evil-mc-initialize)
  )
