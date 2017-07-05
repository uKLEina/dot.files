(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'python-mode-hook 'helm-gtags-mode)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-lg"
 helm-gtags-suggested-key-mapping t
 )

;; Set key bindings
