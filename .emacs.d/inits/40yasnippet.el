(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :bind
  (("<C-tab>" . yas-expand))
  :config
  (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist))

(use-package helm-c-yasnippet
  :defer t
  :bind
  (("C-l y" . helm-yas-complete))
  :config
  (setq helm-yas-space-match-any-greedy t))
