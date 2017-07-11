(use-package winner
  :defer t
  :init
  (winner-mode 1)
  :config
  (defun winner-dwim (arg)
    (interactive "p")
    (let ((func (case arg
                  (16 'sp-new-space-with-default-name)
                  (4 'winner-redo)
                  (1 'winner-undo))))
      (call-interactively func)
      (run-with-timer 0.01 nil 'set 'last-command func)))
  :bind (("C-q" . winner-dwim)
         ;; 元のC-q
         ("C-l C-q" . quoted-insert)))
