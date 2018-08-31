(use-package projectile
  :defer t
  :init
  (bind-key "C-l p e" 'projectile-mode)
  )

(use-package helm-projectile
  :defer t
  :init
  (bind-key "C-l p f" 'helm-projectile-find-file-dwim)
  )
