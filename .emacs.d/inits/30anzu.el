(use-package anzu
  :defer t
  :init
  (global-anzu-mode 1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)
   ("M-#" . anzu-query-replace-at-cursor-thing))
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-use-migemo t)))
