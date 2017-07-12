(use-package ac-helm
  :defer t
  :bind
  (("C-:" . ac-complete-with-helm))
  :config
  (bind-key "C-:" 'ac-complete-with-helm ac-complete-mode-map))
