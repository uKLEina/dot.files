(use-package evil
  :defer t
  :init
  (evil-mode 1)
  :config
  (defun evil-swap-key (map key1 key2)
    ;; MAP中のKEY1とKEY2を入れ替え
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")

  ;; 特定モードではevilを無効化
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'wdired-mode 'normal)

  ;; インデントせずに改行したい
  (defun evil-open-above-without-indent (count)
    "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (evil-insert-newline-above)
    (setq evil-insert-count count
          evil-insert-lines t
          evil-insert-vcount nil)
    (evil-insert-state 1))

  (defun evil-open-below-without-indent (count)
    "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (push (point) buffer-undo-list)
    (evil-insert-newline-below)
    (setq evil-insert-count count
          evil-insert-lines t
          evil-insert-vcount nil)
    (evil-insert-state 1))

  (bind-keys :map evil-insert-state-map
             ("C-t" . other-window-or-split)
             ("C-e" . end-of-line))
  (bind-keys :map evil-normal-state-map
             ("C-t" . other-window-or-split)
             ("C-e" . end-of-line)
             ("J" . (lambda ()
                      (interactive)
                      (evil-scroll-line-down 1)))
             ("K" . (lambda ()
                      (interactive)
                      (evil-scroll-line-up 1)))
             ("M-O" . evil-open-above-without-indent)
             ("M-o" . evil-open-below-without-indent)
             ("M-." . xref-find-definitions)))
