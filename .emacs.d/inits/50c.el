;; 補完はcompanyを使う
(add-hook 'c-mode-common-hook (lambda ()
                                (global-company-mode 1)
                                (global-set-key (kbd "C-M-i") 'company-complete)))
(add-hook 'c-mode-common-hook 'flycheck-mode)
