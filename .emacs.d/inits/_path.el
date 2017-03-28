;;; windows用PATH設定
(when (string-equal system-type "windows-nt")
  (setenv "PATH"
          (concat
           "C:\\msys64\\usr\\local\\bin" ";"
           "C:\\msys64\\mingw64\\bin" ";"
           "C:\\msys64\\usr\\bin" ";"
           (getenv "PATH")))

  (setq exec-path (append '("C:/msys64/usr/local/bin") exec-path))
  (setq exec-path (append '("C:/msys64/mingw64/bin") exec-path))
  (setq exec-path (append '("C:/msys64/usr/bin") exec-path)))


;; (setq exec-path '("d:/data/bin"))
;; (setq exec-path (append exec-path '("d:/data/Python/Scripts")))
;; (setq exec-path (append exec-path '("d:/data/Python")))
;; (setq exec-path (append exec-path '("c:/Users/DPC05009/.cargo/bin")))
;; (setq exec-path (append exec-path '("c:/msys64/usr/local/bin")))
;; (setq exec-path (append exec-path '("c:/msys64/mingw64/bin")))
;; (setq exec-path (append exec-path '("c:/msys64/usr/bin")))
;; (setq exec-path (append exec-path '("c:/Windows/system32")))
;; (setq exec-path (append exec-path '("c:/Windows")))
;; (setq exec-path (append exec-path '("c:/Windows/system32/Wbem")))
;; (setq exec-path (append exec-path '("c:/Windows/system32/WindowsPowerShell/v1.0")))
