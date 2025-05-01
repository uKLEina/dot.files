(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil      ; use-package/straight 等で自分で初期化
      load-prefer-newer nil              ; 起動後に t に戻すならここは nil
      file-name-handler-alist nil)       ; *.el.gz 展開などを一時無効化
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)      ; 16 MiB くらいに戻す
                  gc-cons-percentage 0.1
                  load-prefer-newer t
                  file-name-handler-alist
                  (default-value 'file-name-handler-alist))))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha-background . 90) default-frame-alist)

;; (customize-set-variable 'byte-compile-warnings '(cl-functions))
;; (setopt warning-suppress-types '((package cl)))
