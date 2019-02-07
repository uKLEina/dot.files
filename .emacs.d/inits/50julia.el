(use-package ess-julia
  :mode (("\\.jl\\'" . ess-julia-mode))
  :init
  (add-hook 'ess-julia-mode-hook #'auto-complete-mode)
  :config
  (company-mode -1)
  )
