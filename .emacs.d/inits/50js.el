(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)))

(use-package ac-js2
  :defer t
  :init (add-hook 'js2-mode-hook 'ac-js2-mode))
