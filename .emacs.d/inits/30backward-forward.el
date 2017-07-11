(use-package backward-forward
  :defer t
  :init (backward-forward-mode 1)
  :config
  (setq backward-forward-evil-compatibility-mode t)
  (advice-add 'evil-goto-first-line :before #'backward-forward-push-mark-wrapper)
  (advice-add 'evil-goto-line :before #'backward-forward-push-mark-wrapper)
  (bind-keys :map backward-forward-mode-map
             ("C-l C-a" . backward-forward-previous-location)
             ("<mouse-8>" . backward-forward-previous-location)
             ("C-l C-f" . backward-forward-next-location)
             ("<mouse-9>" . backward-forward-next-location)))
