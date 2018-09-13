(use-package server
  :config (unless (server-running-p)
          (server-start)))

(add-to-list 'load-path (locate-user-emacs-file "elisp"))
