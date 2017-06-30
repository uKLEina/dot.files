(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
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
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-l g c") 'helm-gtags-create-tags)
(define-key helm-gtags-mode-map (kbd "C-l g u") 'helm-gtags-update-tags)
(define-key helm-gtags-mode-map (kbd "C-l g s") 'helm-gtags-select)
