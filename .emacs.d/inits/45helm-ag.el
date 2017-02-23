(setq helm-ag-base-command "rg --vimgrep --no-heading")

;;; 現在のシンボルをデフォルトのクエリにする
(setq helm-ag-insert-at-point 'symbol)
;;; C-M-gはちょうどあいてる
(global-set-key (kbd "C-M-g") 'helm-ag)
(global-set-key (kbd "C-M-k") 'backward-kill-sexp) ;推奨
