(use-package hideshow
  :defer t
  :init
  (defun toggle-hiding (column)
    (interactive "P")
    (if hs-minor-mode
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (toggle-selective-display column)))
  (bind-key "C-l h" 'toggle-hiding)
  )
