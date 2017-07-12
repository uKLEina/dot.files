(use-package ripgrep
  :defer t
  :config
  ;; rgバイナリの位置
  (setq ripgrep-executable "~/.cargo/bin/rg")
  ;; rgに渡すオプション
  (setq ripgrep-arguments '("-S")))
