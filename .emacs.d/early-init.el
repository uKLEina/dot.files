(defvar my/startup-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      load-prefer-newer t
      file-name-handler-alist nil
      site-run-file nil)  ; site-start.elの読み込みを無効

;; エンコーディング（自動検出より前に設定する必要がある）
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(setopt ;; package-enable-at-startup nil
        ;; package-quickstart t
        package-native-compile t)

;; native-compileの最適化
(setq native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors 'silent)

;; UIの無効化（より早いタイミングで）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha-background . 90) default-frame-alist)
(push '(undecorated . nil) default-frame-alist)  ; ウィンドウ装飾

;; 起動後の復元処理を一元化
(defvar my/startup-gc-cons-threshold (* 32 1024 1024))  ; 32MB

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/startup-gc-cons-threshold
                  gc-cons-percentage 0.1
                  ;; init.el中に追加されたエントリ(tramp等)を保持しつつ復元
                  file-name-handler-alist (delete-dups
                                           (append file-name-handler-alist
                                                   my/startup-file-name-handler-alist))
                  native-comp-jit-compilation t))
          t)

;; 警告の抑制
(setq warning-suppress-types '((package reinitialization)
                               (package cl-functions)
                               (comp)))
