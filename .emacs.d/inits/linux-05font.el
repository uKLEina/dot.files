(set-face-attribute 'default nil
                    :family "Ricty Discord"
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :family "Migu 1VS"
                    :height 105)

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

(defun set-face-font-height (size)
  (interactive "nSize: ")
  (set-face-attribute 'default nil
                      :height size))
