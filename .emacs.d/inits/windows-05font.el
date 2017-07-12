(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 105)
(set-face-attribute 'variable-pitch nil
                    :family "Migu 1VS"
                    :height 105)

(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Migu 1M" :size 16))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (font-spec :family "Migu 1M" :size 16))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (font-spec :family "Migu 1M" :size 16))

(add-hook 'text-mode-hook
          '(lambda()
             (buffer-face-set 'variable-pitch)))
(add-hook 'org-mode-hook
          '(lambda()
             (buffer-face-set 'default)))
(add-hook 'yatex-mode-hook
          '(lambda()
             (buffer-face-set 'default)))
(add-hook 'twittering-mode-hook
          '(lambda()
             (buffer-face-set 'variable-pitch)))
(add-hook 'Info-mode-hook
          '(lambda()
             (buffer-face-set 'variable-pitch)))
