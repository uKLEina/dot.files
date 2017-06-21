(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

;;; evil fix
(require 'evil)
(add-hook 'neotree-mode-hook
          (lambda ()
            (evil-make-intercept-map neotree-mode-map)))
