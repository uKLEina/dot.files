(require 'flycheck)
(require 'imenu)
(require 'python-mode)
(setq flycheck-flake8-maximum-line-length 200)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(add-hook 'python-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'py--imenu-create-index-new)
    (setq py--imenu-create-index-p t)))
