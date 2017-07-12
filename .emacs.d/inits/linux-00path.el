(use-package exec-path-from-shell
  :defer t
  :init
  (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-check-startup-files nil))
