(require 'flycheck)
(require 'imenu)
(setq flycheck-flake8-maximum-line-length 200)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(semantic-mode 1)
(add-hook 'python-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'python-imenu-create-index)))
