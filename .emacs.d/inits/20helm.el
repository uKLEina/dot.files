(use-package helm
  :defer t
  :config
  (bind-key "C-h" 'delete-backward-char helm-map)
  (helm-autoresize-mode 1)
  (defun helm-occur-search-forward ()
    (interactive)
    (helm-next-line)
    (helm-execute-persistent-action))
  (define-key helm-map (kbd "C-M-n") 'helm-occur-search-forward)

  (defun helm-occur-search-previous ()
    (interactive)
    (helm-previous-line)
    (helm-execute-persistent-action))
  (define-key helm-map (kbd "C-M-p") 'helm-occur-search-previous)
  (helm-migemo-mode 1)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-for-files)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-files
  :defer t
  :config
  (bind-key "TAB" 'helm-execute-persistent-action helm-find-files-map))

(use-package helm-config
  :defer t)

(use-package helm-mode
  :defer t
  :config
  ;; fuzzy matching
  (custom-set-variables '(helm-mode-fuzzy-match t)
                        '(helm-completion-in-region-fuzzy-match t)))

(use-package helm-smex
  :defer t
  :bind (("M-X" . helm-smex-major-mode-commands))
  :init
  (global-set-key [remap execute-extended-command] #'helm-smex))
