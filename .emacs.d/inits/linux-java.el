(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            (define-key evil-normal-state-map (kbd "M-.") nil)
            ;; meghanada-mode on
            (meghanada-mode t)
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
