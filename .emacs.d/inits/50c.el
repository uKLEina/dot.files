;; 補完はcompanyを使う
(add-hook 'c-mode-common-hook 'company-mode)
(define-key c-mode-map (kbd "C-M-i") 'company-complete)

(add-hook 'c-mode-common-hook 'flycheck-mode)
