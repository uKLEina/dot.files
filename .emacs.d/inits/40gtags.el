(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

;; Set key bindings
(define-key helm-gtags-mode-map (kbd "C-l C-g g") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "C-l C-g c") 'helm-gtags-create-tags)
(define-key helm-gtags-mode-map (kbd "C-l C-g u") 'helm-gtags-update-tags)
(define-key helm-gtags-mode-map (kbd "C-l C-g s") 'helm-gtags-select)
