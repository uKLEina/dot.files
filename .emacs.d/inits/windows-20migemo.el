(use-package migemo
  :init
  (load-library "migemo")
  :config
  (setq migemo-command "C:/Users/NPC05027/bin/cmigemo.exe")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "C:/Users/NPC05027/.emacs.d/dict/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))
