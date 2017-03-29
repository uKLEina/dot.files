(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (require 'ac-php)
             (setq ac-sources  '(ac-source-php))
             (define-key evil-normal-state-map (kbd "M-.") nil)
             (define-key php-mode-map  (kbd "M-.") 'ac-php-find-symbol-at-point)   ;goto define
             (define-key php-mode-map  (kbd "M-,") 'ac-php-location-stack-back   ) ;go back
             ))
