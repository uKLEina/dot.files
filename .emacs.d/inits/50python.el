(use-package elpy
  :init
  ;; elpy-enable just adds elpy-mode to python-mode-hook
  ;; so its ok to call it in init
  (elpy-enable)

  ;; use flycheck instead
  (remove-hook 'elpy-modules 'elpy-module-flymake)

  ;; never use python2 :D
  (custom-set-variables '(elpy-rpc-python-command "python3")
                        '(python-shell-interpreter "python3"))
  :config
  (bind-key "C-l C-v" 'pyvenv-workon elpy-mode)

  ;; use both flake8 and pylint
  ;; flycheck uses only flake8 by default,
  ;; so add pylint after it
  (when (eq system-type 'gnu/linux)
    (flycheck-add-next-checker 'python-flake8 'python-pylint))

  ;; enable checkers after switching venv
  (defun enable_linters (&rest args)
    (flycheck-disable-checker 'python-flake8 t)
    (flycheck-disable-checker 'python-pylint t))
  (advice-add 'pyvenv-workon :after #'enable_linters)

  ;; always pop Docsting window on bottom
  (push '("*Python Doc*" :position bottom :width 30 :noselect t)
        popwin:special-display-config)
  )
