;(add-hook 'after-init-hook #'global-flycheck-mode)
;;; エラーリストはpopwinで下側に出すようにする
(push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
      popwin:special-display-config)
;;; デフォだとelispファイルのチェックが厳しすぎるので変更
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
