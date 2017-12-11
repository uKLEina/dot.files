(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)
         ("C-M-s" . vr/isearch-forward)
         ("C-M-r" . vr/isearch-backward))
  :config
  (use-package visual-regexp-steroids)
  (setq vr/engine 'python))
