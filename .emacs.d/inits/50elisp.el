(use-package flycheck)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
