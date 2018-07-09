(use-package dired
  :defer t
  :config
  (use-package dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq-default dired-omit-files-p nil) ; Buffer-local variable
  (bind-key "C-l C-o" 'dired-omit-mode dired-mode-map)
  ;; fix keybind for SKK
  (setq dired-bind-jump nil)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-AFDlh --group-directories-first")
    ;; ファイルのあるディレクトリを起点に Nautilus を開く
    (defun file-open-nautilus ()
      (interactive)
      (call-process "nautilus" nil nil nil "--no-desktop" "-n"
                    (or (file-name-directory buffer-file-name)
                        default-directory)))
    (bind-key "C-l C-e" 'file-open-nautilus)

    ;; http://qiita.com/items/2620874c802db60c99f9
    (defun dired-open-nautilus ()
      (interactive)
      (call-process "nautilus" nil 0 nil (dired-current-directory)))
    (bind-key "e" 'dired-open-nautilus dired-mode-map))
  (when (eq system-type 'windows-nt)
    (setq ls-lisp-dirs-first t))
  (use-package dired-quick-sort
    :commands (hydra-dired-quick-sort/body)
    :init
    (bind-key "S" 'hydra-dired-quick-sort/body dired-mode-map)))

(use-package all-the-icons-dired
  :defer t
  :diminish all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  )
