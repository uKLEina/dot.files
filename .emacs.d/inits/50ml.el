(use-package tuareg
  :defer t
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)
         ("\\.mly\\'" . tuareg-mode)
         ("\\.mll\\'" . tuareg-mode)
         ("\\.mlp\\'" . tuareg-mode))
  :config
  (custom-set-variables '(tuareg-default-indent 4)))
