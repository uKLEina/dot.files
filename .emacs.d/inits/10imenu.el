(with-eval-after-load "imenu-list"
  (define-key imenu-list-major-mode-map (kbd "j") 'next-line)
  (define-key imenu-list-major-mode-map (kbd "k") 'previous-line))
