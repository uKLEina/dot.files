(use-package ispell
  :defer t
  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
