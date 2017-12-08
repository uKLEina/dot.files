(use-package meghanada
  :defer t
  :commands
  (meghanada-mode)
  :init
  (add-hook 'java-mode-hook
                  (lambda ()
                    (meghanada-mode t)
                    (smartparens-mode t)))
  :config
  (evil-make-overriding-map meghanada-mode-map))
