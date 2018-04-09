(use-package elpy
  :defer t
  :init
  (elpy-enable)
  (custom-set-variables '(elpy-rpc-python-command "python3")
                        '(python-shell-interpreter "python3"))
  :config
  (bind-key "C-l C-v" 'pyvenv-workon elpy-mode))
