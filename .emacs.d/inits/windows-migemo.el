(require 'migemo)
(setq migemo-command "cmigemo.exe")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "D:\\data\\.emacs.d\\migemo-dict\\cp932\\migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
(setq migemo-coding-system 'cp932)
(load-library "migemo")
(migemo-init)

;;; ガイド
