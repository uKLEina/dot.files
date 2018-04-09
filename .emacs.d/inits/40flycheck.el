(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (flycheck-pos-tip-mode)
  (smartrep-define-key
      flycheck-mode-map "C-c !"
    '(("n" . flycheck-next-error)
      ("p" . flycheck-previous-error)))
  (push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
        popwin:special-display-config)
  )
