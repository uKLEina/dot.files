(use-package highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 0.75)
  :bind (("M-#" . highlight-symbol-query-replace)
         ("C-l C-s" . highlight-symbol)))
