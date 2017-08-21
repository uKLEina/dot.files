(use-package ac-octave
  :defer t
  :mode (("\\.m\\'" . octave-mode))
  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (setq ac-sources '(ac-complete-octave))))
  (add-hook 'octave-mode-hook 'smartparens-mode)
  )
