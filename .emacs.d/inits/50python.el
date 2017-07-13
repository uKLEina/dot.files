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
    (setq flycheck-flake8-maximum-line-length 200)
    (flycheck-add-next-checker 'python-flake8 'python-pylint)))

(use-package virtualenvwrapper
  :defer t)

(use-package py-autopep8
  :defer t
  :config
  (setq py-autopep8-options '("--max-line-length=200")))

(use-package jedi
  :defer t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq ac-sources
        (delete 'ac-source-words-in-same-mode-buffers ac-sources))
  (setq jedi:complete-on-dot t)
  (evil-make-intercept-map jedi-mode-map)
  (setq jedi:use-shortcuts t)
  (bind-key "C-M-i" 'jedi:complete python-mode-map)
  (push '("*jedi:doc*" :position bottom :width 30)
        popwin:special-display-config))

(use-package imenu
  :defer t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'py--imenu-create-index-new)
              (setq py--imenu-create-index-p t))))
