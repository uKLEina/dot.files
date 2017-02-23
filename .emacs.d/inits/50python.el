(require 'python-mode)
(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)

(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=1000"))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:environment-root "~/opt/anaconda3/envs/tanawari")
(setq jedi:complete-on-dot t)
;; (define-key python-mode-map (kbd "<C-tab>") 'jedi:complete)
(add-hook 'python-mode-hook #'smartparens-mode)
