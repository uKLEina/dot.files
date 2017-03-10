(require 'helm-etags+)

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'python-mode-hook 'turn-on-ctags-auto-update-mode)

(autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
(global-set-key (kbd "C-c E") 'ctags-update)

(global-set-key (kbd "C-l C-d") 'helm-etags+-select)
;;list all visited tags
(global-set-key (kbd "M-*") 'helm-etags+-history)
;;go back directly
(global-set-key (kbd "M-,") 'helm-etags+-history-action-go-back)
;;go forward directly
(global-set-key (kbd "M-/") 'helm-etags+-history-action-go-forward)

;;; backward-forward
(advice-add 'helm-etags+-select :before #'backward-forward-push-mark-wrapper)
