(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key (kbd "C-l C-t") 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("en" . "ja") ("ja" . "en")))
;;; 翻訳結果はpopwinで出す
(push '("*Google Translate*" :position bottom :width 30)
      popwin:special-display-config)
