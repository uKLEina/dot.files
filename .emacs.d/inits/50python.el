(require 'python-mode)

(require 'flycheck-pyflakes)
(add-hook 'python-mode-hook 'flycheck-mode)

(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=1000"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(require 'jedi)
(setq jedi:environment-root "~/.virtualenvs/tanawari")
(setq jedi:complete-on-dot t)
;; (define-key python-mode-map (kbd "<C-tab>") 'jedi:complete)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq ac-sources
                  (delete 'ac-source-words-in-same-mode-buffers ac-sources))))

(add-hook 'python-mode-hook #'smartparens-mode)
