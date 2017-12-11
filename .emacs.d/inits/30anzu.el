(use-package anzu
  :init
  (global-anzu-mode 1)
  :config
  (bind-key "M-#" 'anzu-query-replace-at-cursor-thing)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-use-migemo t)))
