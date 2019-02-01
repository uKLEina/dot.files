(use-package ess-site
  :mode (("\\.jl\\'" . ess-julia-mode))
  :init
  (add-hook 'ess-julia-mode-hook #'auto-complete-mode)
  )
