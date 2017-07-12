(use-package helm-ag
  :defer t
  :bind (("C-M-g" . helm-ag)
         ("C-M-k" . backward-kill-sexp))
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  ;; 現在のシンボルをデフォルトのクエリにする
  (setq helm-ag-insert-at-point 'symbol))
