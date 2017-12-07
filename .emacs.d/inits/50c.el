(use-package rtags
  :load-path "/usr/local/share/emacs/site-lisp/rtags/"
  :init
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))
  (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (rtags-is-indexed)
                (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                (local-set-key (kbd "M-;") 'rtags-find-symbol)
                (local-set-key (kbd "M-@") 'rtags-find-references)
                (local-set-key (kbd "M-,") 'rtags-location-stack-back))))
  :config
  (company-mode-on)
  (auto-complete-mode -1)
  (evil-make-overriding-map c-mode-base-map)
  (evil-make-overriding-map c++-mode-map)
  (evil-make-overriding-map c-mode-map)
  (custom-set-variables
   '(rtags-completions-enabled t)
   '(rtags-autostart-diagnostics t)
   '(rtags-display-result-backend "Helm"))
  (use-package company-rtags)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-rtags))
  (use-package flycheck-rtags)
  )

(use-package irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (company-mode-on)
  (auto-complete-mode -1)
  (use-package company-irony)
  (company-irony-setup-begin-commands)
  (use-package company-irony-c-headers)
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-irony-c-headers company-irony) ))
  (use-package flycheck-irony)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
