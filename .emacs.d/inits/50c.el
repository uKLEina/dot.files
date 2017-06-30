(require 'cc-mode)
;; 補完はcompanyを使う
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)
            (global-company-mode 1)
            (global-set-key (kbd "C-M-i") 'company-complete)))
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
