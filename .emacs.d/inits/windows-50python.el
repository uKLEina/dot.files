;;; windows だとなぜか flake8 が動かないので pyflakes を使う
;;; autopep8 は動くので擬似的に flake8 になってるはず
(use-package flycheck-pyflakes
  :defer t
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  :config
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (flycheck-add-next-checker 'python-pyflakes 'python-pylint))
