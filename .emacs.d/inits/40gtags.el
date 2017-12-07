(use-package helm-gtags
  :defer t
  :init
  (add-hook 'python-mode-hook 'helm-gtags-mode)
  :config
  (setq
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t)
  (unbind-key "M-." evil-normal-state-map)
  (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map))
