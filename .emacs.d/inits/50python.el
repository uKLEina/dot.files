(use-package elpy
  :defer t
  :init
  (elpy-enable)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (custom-set-variables '(elpy-rpc-python-command "python3")
                        '(python-shell-interpreter "python3"))
  (defun python-mode-setup ()
    "hook function for `python-mode'."
    (auto-complete-mode -1)
    (electric-operator-mode +1))
  (add-hook 'python-mode-hook 'python-mode-setup)
  :config
  (bind-key "C-l C-v" 'pyvenv-workon elpy-mode)
  (when (eq system-type 'gnu/linux)
    (flycheck-add-next-checker 'python-flake8 'python-pylint))
  (push '("*Python Doc*" :position bottom :width 30 :noselect t)
        popwin:special-display-config)
  )
