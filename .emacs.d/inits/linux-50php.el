(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :config
  (evil-make-intercept-map php-mode-map)
  )
