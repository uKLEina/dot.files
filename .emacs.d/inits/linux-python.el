(require 'flycheck)
(setq flycheck-flake8-maximum-line-length 200)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
