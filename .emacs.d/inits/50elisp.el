;;; auto-complete
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;;; デフォだとelispファイルのチェックが厳しすぎるので変更
(require 'flycheck)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
