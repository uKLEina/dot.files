(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :config
  (bind-key "C-l i" 'clang-format-region c++-mode-map)
  (evil-make-intercept-map c++-mode-map))

(use-package irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (evil-make-intercept-map irony-mode-map)
  (setq company-idle-delay 0)
  (bind-key "C-M-i" 'company-complete c++-mode-map)
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
  (setq c-default-style "k&r")
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2))

(use-package company-irony
  :defer t
  :init
  (add-hook 'irony-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-irony))))

(use-package irony-eldoc
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package cmake-ide
  :defer t
  :init
  (add-hook 'irony-mode-hook 'cmake-ide-setup))

(use-package flycheck-irony
  :defer t
  :init
  (add-hook 'irony-mode-hook #'flycheck-irony-setup))

(use-package clang-format
  :defer t
  )

(use-package company-rtags
  :commands (company-rtags)
  :init
  (add-hook 'rtags-mode-hook
            (lambda ()
              (when (rtags-is-indexed)
                (setq rtags-autostart-diagnostics t)
                (rtags-diagnostics)
                (setq rtags-completions-enabled t)
                (add-to-list 'company-backends 'company-rtags)))))

(use-package rtags
  :defer t
  :commands (rtags-is-indexed)
  :init
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  :config
  (when (rtags-is-indexed)
    (helm-gtags-mode -1)
    (use-package helm-rtags)
    (custom-set-variables '(rtags-display-result-backend "Helm"))
    (custom-set-variables '(rtags-popup-results-buffer t))
    (unbind-key "M-." evil-normal-state-map)
    (bind-key "M-." 'rtags-find-symbol-at-point c++-mode-map)))

(use-package company-c-headers
  :defer t
  :init
  (add-hook 'company-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-c-headers)))
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/6/"))
