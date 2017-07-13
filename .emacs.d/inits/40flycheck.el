(use-package flycheck
  :defer t
  :init
  (add-hook 'irony-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'meghanada-mode-hook 'flycheck-mode)
  :config
  (flycheck-pos-tip-mode)
  (smartrep-define-key
      flycheck-mode-map "C-c !"
    '(("n" . flycheck-next-error)
      ("p" . flycheck-previous-error)))
  (push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
        popwin:special-display-config))
