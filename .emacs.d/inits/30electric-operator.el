(use-package electric-operator
  :diminish
  :init
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (add-hook 'ess-julia-mode-hook #'electric-operator-mode))
