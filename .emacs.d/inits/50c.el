(defun c/c++-mode-setup ()
  "hook function to setup `c-mode' and `c++-mode'."
  (projectile-mode)
  (hs-minor-mode 1)
  (auto-complete-mode -1)
  )
(add-hook 'c-mode-hook #'c/c++-mode-setup)
(add-hook 'c++-mode-hook #'c/c++-mode-setup)

(use-package rtags
  :load-path "/usr/local/share/emacs/site-lisp/rtags/"
  :init
  (defun c/c++-mode-rtags-setup ()
    "hook function to setup rtags things for `c-mode' and `c++-mode'."
    (when (rtags-is-indexed)
      (rtags-start-process-unless-running)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
      (local-set-key (kbd "M-,") 'rtags-location-stack-back)
      (local-set-key (kbd "M-@") 'rtags-find-references)
      (local-set-key (kbd "C-M-.") 'rtags-next-match)
      (local-set-key (kbd "C-M-,") 'rtags-next-match)
      )
    )
  (add-hook 'c-mode-hook #'c/c++-mode-rtags-setup)
  (add-hook 'c++-mode-hook #'c/c++-mode-rtags-setup)
  :config
  (evil-make-overriding-map c++-mode-map)
  (evil-make-overriding-map c-mode-map)
  (when (rtags-is-indexed)
    (custom-set-variables
     '(rtags-completions-enabled t)
     '(rtags-autostart-diagnostics t))
    (use-package company-rtags)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-rtags))
    (use-package flycheck-rtags)
    (flycheck-select-checker 'rtags)
    (push '("*RTags*" :position bottom :noselect t)
          popwin:special-display-config))
  )

(use-package irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (use-package company-irony)
  (company-irony-setup-begin-commands)
  (use-package company-irony-c-headers)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  (use-package flycheck-irony)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )

(use-package clang-format
  :defer t
  :init
  (bind-key "C-l i" 'clang-format-buffer c++-mode-map)
  (bind-key "C-l i" 'clang-format-buffer c-mode-map)
  )

(use-package hideif
  :defer t
  :init
  (add-hook 'c++-mode-hook 'hide-ifdef-mode)
  (add-hook 'c-mode-hook 'hide-ifdef-mode)
  )
