;; ファイルのあるディレクトリを起点に Nautilus を開く
(defun file-open-nautilus ()
  (interactive)
  (call-process "nautilus" nil nil nil "--no-desktop" "-n"
                (or (file-name-directory buffer-file-name)
                    default-directory)))

(global-set-key (kbd "C-l C-e") 'file-open-nautilus)

;; http://qiita.com/items/2620874c802db60c99f9
(defun dired-open-nautilus ()
  (interactive)
  (call-process "nautilus" nil 0 nil (dired-current-directory)))
(define-key dired-mode-map "e" 'dired-open-nautilus)
