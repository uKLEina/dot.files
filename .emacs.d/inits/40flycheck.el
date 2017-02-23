(add-hook 'after-init-hook #'global-flycheck-mode)
;;; popwinで下側に出すようにする
(push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
      popwin:special-display-config)
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
