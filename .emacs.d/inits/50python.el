(use-package python-mode
  :defer t
  :mode (("\\.py\\'" . python-mode))
  :config
  (smartrep-define-key
      python-mode-map "C-c"
    '((">" . py-shift-right)
      ("<" . py-shift-left)))
  (bind-keys :map python-mode-map
             ("C-l C-v" . venv-workon)
             ("C-l i" . py-autopep8-buffer))
  (when (eq system-type 'gnu/linux)
    (flycheck-add-next-checker 'python-flake8 'python-pylint))
  )

(use-package virtualenvwrapper
  :defer t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs")
  )

(use-package py-autopep8
  :defer t
  :config
  (setq py-autopep8-options '("--max-line-length=200")))

(use-package jedi
  :defer t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (custom-set-variables '(jedi:tooltip-method nil))
  (setq jedi:complete-on-dot t)
  (evil-make-intercept-map jedi-mode-map)
  (setq jedi:use-shortcuts t)
  (push '("*jedi:doc*" :position bottom :width 30)
        popwin:special-display-config)
  )

(use-package imenu
  :defer t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'py--imenu-create-index-new)
              (setq py--imenu-create-index-p t))))
