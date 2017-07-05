(require 'cc-mode)
(require 'company)
(require 'rtags)
(require 'helm-rtags)
(require 'clang-format)
(require 'evil)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; c-mode-common-hook だと php-mode も入っちゃうので
(defun c/c++-mode-hook-func ()
  "Hook function for `c-mode' and `c++-mode'."
  (when (memq major-mode '(c-mode c++-mode))
    'irony-mode))

(eval-after-load "irony"
  '(progn
     (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
     (add-to-list 'company-backends 'company-irony)
     (add-hook 'c-mode-hook 'c/c++-mode-hook-func)
     (add-hook 'c++-mode-hook 'irony-mode)
     (add-hook 'c++-mode-hook (lambda () (evil-make-intercept-map c++-mode-map)))
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (add-hook 'irony-mode-hook 'irony-eldoc)
     (add-hook 'irony-mode-hook 'cmake-ide-setup)
     (add-hook 'irony-mode-hook 'flycheck-mode)
     (add-hook 'irony-mode-hook
               (lambda ()
                 (setq c-default-style "k&r")
                 (setq indent-tabs-mode nil)
                 (setq c-basic-offset 2)
                 (company-mode 1)
                 (setq company-idle-delay 0)))))


(define-key c++-mode-map (kbd "C-M-i") 'company-complete)
(define-key c++-mode-map (kbd "C-l i") 'clang-format-buffer)
(define-key c++-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
(custom-set-variables '(rtags-display-result-backend "Helm"))
(custom-set-variables '(rtags-popup-results-buffer t))
