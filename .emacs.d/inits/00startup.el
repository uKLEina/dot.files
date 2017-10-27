(use-package server
  :defer t
  :commands (server-running-p)
  :init (unless (server-running-p)
          (server-start)))

(add-to-list 'load-path (locate-user-emacs-file "elisp"))
