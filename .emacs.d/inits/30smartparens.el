(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'c-mode-hook #'smartparens-mode)
  (add-hook 'c++-mode-hook #'smartparens-mode)
  (add-hook 'java-mode-hook #'smartparens-mode))
