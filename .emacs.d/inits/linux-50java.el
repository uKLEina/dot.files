(use-package meghanada
  :defer t
  :init (add-hook 'java-mode-hook
                  (lambda ()
                    (meghanada-mode t)))
  :config
  (evil-make-intercept-map meghanada-mode-map)
  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
  (define-key meghanada-mode-map (kbd "C-M-i") 'company-complete))
