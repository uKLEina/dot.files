(use-package elpy
  :defer t
  :init
  (elpy-enable)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (custom-set-variables '(elpy-rpc-python-command "python3")
                        '(python-shell-interpreter "python3"))
  :config
  (bind-key "C-l C-v" 'pyvenv-workon elpy-mode)
  (when (eq system-type 'gnu/linux)
    (flycheck-add-next-checker 'python-flake8 'python-pylint))
  )
