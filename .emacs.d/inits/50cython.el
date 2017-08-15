(use-package cython-mode
  :defer t
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)
         ("\\.pxi\\'" . cython-mode)))

(require 'flycheck-cython)
(add-hook 'cython-mode-hook 'flycheck-mode)
