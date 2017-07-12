(use-package dired
  :defer t
  :config
  ;; fix keybind for SKK
  (setq dired-bind-jump nil)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
  ;; ファイルのあるディレクトリを起点に Nautilus を開く
  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-AFLDlh --group-directories-first")
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
    (setq ls-lisp-dirs-first t)))

(use-package dired-quick-sort
  :defer t
  :init (dired-quick-sort-setup))
