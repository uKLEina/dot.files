;;; windows用PATH設定
;;; emacsではgnu packageを使いたいのでmsys以下をPATHの前の方に持ってくる
;;; 要するにgnu findを使いたい
(when (string-equal system-type "windows-nt")
  (setenv "PATH"
          (concat
           "C:\\msys64\\usr\\local\\bin" ";"
           "C:\\msys64\\mingw64\\bin" ";"
           "C:\\msys64\\usr\\bin" ";"
           (getenv "PATH")))

  (setq exec-path (append '("C:/msys64/usr/bin") exec-path))
  (setq exec-path (append '("C:/msys64/mingw64/bin") exec-path))
  (setq exec-path (append '("C:/msys64/usr/local/bin") exec-path)))
