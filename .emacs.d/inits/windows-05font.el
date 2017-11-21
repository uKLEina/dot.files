(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 110)
(set-face-attribute 'variable-pitch nil
                    :family "Migu 1VS"
                    :height 110)

(add-to-list 'face-font-rescale-alist '("Migu 1M" . 1.1))
(set-fontset-font nil 'japanese-jisx0208
                  (font-spec :family "Migu 1M"))
(set-fontset-font nil 'japanese-jisx0212
                  (font-spec :family "Migu 1M"))
(set-fontset-font nil 'katakana-jisx0201
                  (font-spec :family "Migu 1M"))

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
