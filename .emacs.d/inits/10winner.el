(winner-mode 1)

(defun winner-dwim (arg)
  (interactive "p")
  (let ((func (case arg
                (16 'sp-new-space-with-default-name)
                (4 'winner-redo)
                (1 'winner-undo))))
    (call-interactively func)
    (run-with-timer 0.01 nil 'set 'last-command func)))

(global-set-key (kbd "C-q") 'winner-dwim)
;; 元のC-q
(global-set-key (kbd "C-x C-q") 'quoted-insert)
