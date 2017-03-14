(backward-forward-mode 1)

(setq backward-forward-evil-compatibility-mode t)

(define-key backward-forward-mode-map (kbd "C-l C-a") 'backward-forward-previous-location)
(define-key backward-forward-mode-map (kbd "C-l C-f") 'backward-forward-next-location)
(advice-add 'evil-goto-first-line :before #'backward-forward-push-mark-wrapper)
(advice-add 'evil-goto-line :before #'backward-forward-push-mark-wrapper)
