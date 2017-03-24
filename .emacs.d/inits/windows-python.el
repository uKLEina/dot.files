;;; windows だとflake8が動かない……
(require 'flycheck-pyflakes)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
