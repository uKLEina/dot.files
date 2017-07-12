(defadvice update-buffer-local-cursor-color
  (around evil-update-buffer-local-cursor-color-in-insert-state activate)
  ;; SKKによるカーソル色変更を, 挿入ステートかつ日本語モードの場合に限定
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (when (and (eq evil-state 'insert) (bound-and-true-p skk-j-mode))
    ad-do-it))
(defadvice evil-refresh-cursor
  (around evil-refresh-cursor-unless-skk-mode activate)
  ;; Evilによるカーソルの変更を, 挿入ステートかつ日本語モードではない場合に限定
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (unless (and (eq evil-state 'insert) (bound-and-true-p skk-j-mode))
    ad-do-it))
