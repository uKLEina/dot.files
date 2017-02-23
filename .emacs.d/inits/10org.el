(define-key global-map (kbd "C-l o l") 'org-store-link)
(define-key global-map (kbd "C-l o a") 'org-agenda)
(define-key global-map (kbd "C-l o r") 'org-remember)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/Documents/todo")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "todo.org")
;;; DONEの日時を記録
(setq org-log-done 'time)

;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))
;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)
;; 標準の祝日を利用しない
(setq calendar-holidays nil)

;;; LaTeX連携
(setq org-export-latex-classes nil)
(add-to-list 'org-export-latex-classes
  '("jarticle"
    "\\documentclass[a4j]{jarticle}"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
))
