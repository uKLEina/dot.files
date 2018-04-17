(use-package projectile
  :defer t
  )

(use-package helm-projectile
  :defer t
  :init
  (bind-key "C-l p f" 'helm-projectile-find-file-dwim)
  (bind-key "C-l p f" 'helm-projectile-find-file-dwim)
  )
