(use-package smartparens
  :defer t
  :init
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'c-mode-hook #'smartparens-mode)
  (add-hook 'c++-mode-hook #'smartparens-mode)
  (add-hook 'java-mode-hook #'smartparens-mode))
