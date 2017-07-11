(use-package imenu-list
  :defer t
  :config
  (bind-keys :map imenu-list-major-mode-map
             ("j" . next-line)
             ("k" . previous-line)))
