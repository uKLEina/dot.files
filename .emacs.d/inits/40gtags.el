(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

;; Set key bindings
(define-key helm-gtags-mode-map (kbd "C-l d") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "C-l C-d c") 'helm-gtags-create-tags)
(define-key helm-gtags-mode-map (kbd "C-l C-d u") 'helm-gtags-update-tags)
(define-key helm-gtags-mode-map (kbd "C-l C-d s") 'helm-gtags-select)
