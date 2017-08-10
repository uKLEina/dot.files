(use-package ac-octave
  :defer t
  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (setq ac-sources '(ac-complete-octave)))))
