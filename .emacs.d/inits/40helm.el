;; fix keybind for SKK
(setq dired-bind-jump nil)

(require 'helm)
(require 'helm-config)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x b") 'helm-for-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-autoresize-mode 1)

;; fuzzy matching
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(defun helm-occur-search-forward ()
  (interactive)
  (helm-next-line)
  (helm-execute-persistent-action))
(define-key helm-map (kbd "C-n") 'helm-occur-search-forward)

(defun helm-occur-search-previous ()
  (interactive)
  (helm-previous-line)
  (helm-execute-persistent-action))
(define-key helm-map (kbd "C-p") 'helm-occur-search-previous)

;;; helm-smex
(global-set-key [remap execute-extended-command] #'helm-smex)
(global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)

(require 'migemo)
(helm-migemo-mode 1)
