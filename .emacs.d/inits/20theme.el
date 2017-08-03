(use-package zenburn
  :defer t
  :init (load-theme 'zenburn t)
  :config
  )

(set-face-foreground 'mode-line "NavajoWhite1")
(set-face-foreground 'mode-line-inactive "NavajoWhite3")

;; mode-line color as evil state
; normal
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (set-face-background 'mode-line "gray15")))
; insert
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (set-face-background 'mode-line "DarkOliveGreen")))
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (set-face-background 'mode-line "gray15")))
; visual
(add-hook 'evil-visual-state-entry-hook
          (lambda ()
            (set-face-background 'mode-line "DarkCyan")))
(add-hook 'evil-visual-state-exit-hook
          (lambda ()
            (set-face-background 'mode-line "gray15")))
; emacs
(add-hook 'evil-emacs-state-entry-hook
          (lambda ()
            (set-face-background 'mode-line "BlueViolet")))
(add-hook 'evil-emacs-state-exit-hook
          (lambda ()
            (set-face-background 'mode-line "gray15")))
(add-hook 'switch-buffer-functions
          (lambda (prev cur)
            (let ((mode-line-color (pcase evil-state
                                     (`normal "gray15")
                                     (`insert "DarkOliveGreen")
                                     (`visual "DarkCyan")
                                     (`emacs "BlueViolet"))))
              (set-face-background 'mode-line mode-line-color))))
