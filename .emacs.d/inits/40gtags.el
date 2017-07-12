(use-package helm-gtags
  :defer t
  :init
  (add-hook 'python-mode-hook 'helm-gtags-mode)
  :config
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-lg"
   helm-gtags-suggested-key-mapping t))
