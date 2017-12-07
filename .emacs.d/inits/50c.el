(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :config
  (custom-set-variables '(c-default-style "k&r"))
  (setq indent-tabs-mode nil)
  (use-package clang-format
    :defer t
    :init
    (eval-after-load 'c-mode
      (bind-key "C-l i" 'clang-format-region c-mode-map))
    (eval-after-load 'c++-mode
      (bind-key "C-l i" 'clang-format-region c++-mode-map)))
  (use-package rtags
    :load-path "/usr/local/share/emacs/site-lisp/rtags/"
    :config
    (rtags-start-process-unless-running)
    (custom-set-variables
     '(rtags-display-result-backend "Helm")
     '(rtags-popup-results-buffer t))
    (unbind-key "M-." evil-normal-state-map)
    (bind-keys :map rtags-mode-map
               ("M-." . rtags-find-symbol-at-point)
               ("M-]" . rtags-find-references-at-point)
               ("M-," . rtags-location-stack-back)
               ("M-[" . rtags-next-match)
               ("M-@" . rtags-previous-match))
    (use-package company-rtags
      :config
      (custom-set-variables
       '(rtags-autostart-diagnostics t)
       '(rtags-completions-enabled t))
      (rtags-diagnostics)
      (add-to-list 'company-backends 'company-rtags))
    (use-package cmake-ide
      :config
      (cmake-ide-setup)
      (bind-key "<f9>" 'cmake-ide-compile rtags-mode-map)
      ))
  )

(use-package irony
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-mode)
  :config
  (bind-key "C-M-i" 'company-complete irony-mode-map)
  (use-package company-irony
    :config (add-to-list 'company-backends 'company-irony))
  (use-package company-c-headers
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/6/")))

(use-package irony-eldoc
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package flycheck-irony
  :defer t
  :init
  (add-hook 'irony-mode-hook #'flycheck-irony-setup))
