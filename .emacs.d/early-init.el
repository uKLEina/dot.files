(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      package-enable-at-startup nil
      load-prefer-newer nil
      file-name-handler-alist nil
      site-run-file nil)  ; site-start.elの読み込みを無効

;; native-compileの最適化
(setq native-comp-deferred-compilation t
      native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors 'silent)

;; UIの無効化（より早いタイミングで）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha-background . 90) default-frame-alist)
(push '(undecorated . nil) default-frame-alist)  ; ウィンドウ装飾

;; 起動後の復元処理を一元化
(defvar my/startup-gc-cons-threshold (* 32 1024 1024))  ; 32MB
(defvar my/startup-file-name-handler-alist file-name-handler-alist)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/startup-gc-cons-threshold
                  gc-cons-percentage 0.1
                  load-prefer-newer t
                  file-name-handler-alist my/startup-file-name-handler-alist
                  native-comp-jit-compilation t))
          t)

;; 警告の抑制
(setq warning-suppress-types '((package reinitialization)
                               (package cl-functions)
                               (comp)))

;; パッケージの事前最適化
(setq package-quickstart t
      package-native-compile t)
