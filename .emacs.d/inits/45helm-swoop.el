(use-package helm-swoop
  :defer t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (bind-keys :map helm-swoop-map
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line)
             ("M-i" . helm-multi-swoop-all-from-helm-swoop))
  (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map))
