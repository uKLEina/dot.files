;;; c-mode-common-hook だと php-mode も入っちゃうので
(defun c/c++-mode-hook-func ()
  "Hook function for `c-mode' and `c++-mode'."
  (when (memq major-mode '(c-mode c++-mode))
    'irony-mode))

(require 'company)
(eval-after-load "irony"
  '(progn
     (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
     (add-to-list 'company-backends 'company-irony)
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (add-hook 'c-mode-hook 'c/c++-mode-hook-func)
     (add-hook 'c++-mode-hook 'c/c++-mode-hook-func)))
