(use-package ispell
  :defer t
  :init
  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (ac-ispell-setup)
  )

(use-package ac-ispell
  :defer t)

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :config
  (unbind-key "C-M-i" flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map)
  (unbind-key "C-," flyspell-mode-map))

(use-package flyspell-correct
  :defer t
  :diminish flyspell-correct-auto-mode
  :init
  (bind-key "C-M-;" 'flyspell-correct-word-generic)
  :config
  (use-package flyspell-correct-helm)
  (unbind-key "C-;" flyspell-mode-map))
