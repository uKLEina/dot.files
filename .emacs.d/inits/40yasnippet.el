;; TABで補完とか略語展開とかうまくdwimできないのでキーバインド変える&helm使う
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "<C-tab>") 'yas-expand)
(global-set-key (kbd "C-l y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode 1)
