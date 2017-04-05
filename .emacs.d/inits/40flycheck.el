(require 'flycheck)
;(add-hook 'after-init-hook #'global-flycheck-mode)
;;; エラーリストはpopwinで下側に出すようにする
(push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
      popwin:special-display-config)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode)
  (smartrep-define-key
      flycheck-mode-map "C-c !"
    '(("n" . flycheck-next-error)
      ("p" . flycheck-previous-error))))
