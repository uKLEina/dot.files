(use-package php-mode
  :mode (("\\.php\\'" . php-mode))
  :config
  (auto-complete-mode t)
  (use-package ac-php)
  (setq ac-sources '(ac-source-php))
  (evil-make-intercept-map php-mode-map)
  (bind-keys :map php-mode-map
             ("M-." . ac-php-find-symbol-at-point)
             ("M-," . ac-php-location-stack-back)))
