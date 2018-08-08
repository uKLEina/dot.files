(use-package neotree
  :defer t
  :bind (("<f8>" . neotree-toggle))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-window-fixed-size nil)
  :config
  (evil-make-intercept-map neotree-mode-map)
  )
