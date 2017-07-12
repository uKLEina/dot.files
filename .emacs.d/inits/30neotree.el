(use-package neotree
  :defer t
  :bind (("<f8>" . neotree-toggle))
  :config
  (evil-make-intercept-map neotree-mode-map))
