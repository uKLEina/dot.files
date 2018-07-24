(use-package rust-mode
  :defer t
  :init
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook #'electric-operator-mode)
  :custom
  (rust-format-on-save t)
  )

(use-package racer
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  :config
  (unbind-key "M-." evil-normal-state-map)
  (bind-key "M-." 'racer-find-definition rust-mode-map)
  )

(use-package flycheck-rust
  :defer t
  :init
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  )
