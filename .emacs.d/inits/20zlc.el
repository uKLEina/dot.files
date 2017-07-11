(use-package zlc
  :defer t
  :init (zlc-mode t)
  :config
  (bind-keys :map minibuffer-local-map
             ("<down>" . zlc-select-next-vertical)
             ("<up>" . zlc-select-previous-vertical)
             ("<right>" . zlc-select-next)
             ("<left>" . zlc-select-previous)
             ("C-c" . zlc-reset)))
