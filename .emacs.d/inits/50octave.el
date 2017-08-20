(use-package ac-octave
  :defer t
  :mode (("\\.ml\\'" . tuareg-mode))
  :mode
  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (setq ac-sources '(ac-complete-octave)))))
