;;; windows だとなぜか flake8 が動かないので pyflakes を使う
;;; autopep8 は動くので擬似的に flake8 になってるはず
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
