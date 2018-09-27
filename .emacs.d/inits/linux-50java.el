(use-package meghanada
  :defer t
  :init
  (defun java-mode-setup ()
    "hook function for `java-mode'."
    (meghanada-mode t)
    (smartparens-mode t)
    (flycheck-mode +1))
  (add-hook 'java-mode-hook 'java-mode-setup)
  :config
  (bind-key "C-l i" 'meghanada-code-beautify meghanada-mode-map)
  (evil-make-overriding-map meghanada-mode-map))
