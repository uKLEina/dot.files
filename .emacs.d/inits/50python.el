(require 'python-mode)

;;; IDEてきなやついろいろ
(add-hook 'python-mode-hook 'flycheck-mode)

(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=99"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(require 'jedi)
;; きちゃないけどjedi用の環境を直接書いちゃう
(setq jedi:environment-root "~/.virtualenvs/tanawari")
(setq jedi:complete-on-dot t)
(define-key python-mode-map (kbd "C-M-i") 'jedi:complete)
;; pathも直接書いちゃう
(add-hook 'python-mode-hook
          (lambda ()
            (cond ((file-exists-p "~/.virtualenvs/tanawari/Scripts")
                   (setq exec-path (append '("~/.virtualenvs/tanawari/Scripts") exec-path))
                   (setenv "PATH" (concat "~/.virtualenvs/tanawari/Scripts:" (getenv "PATH"))))
                  ((file-exists-p "~/.virtualenvs/tanawari/bin")
                   (setq exec-path (append '("~/.virtualenvs/tanawari/bin") exec-path))
                   (setenv "PATH" (concat "~/.virtualenvs/tanawari/bin:" (getenv "PATH")))))))
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq ac-sources
                  (delete 'ac-source-words-in-same-mode-buffers ac-sources))))

;; jediのdocstring参照をpopwinで出すようにする
(require 'popwin)
(push '("*jedi:doc*" :position bottom :width 30)
      popwin:special-display-config)

;;; M-. を使いたいのでevilのキーバインド解除
(require 'evil)
(add-hook 'python-mode-hook
          (lambda ()
            (define-key evil-normal-state-map (kbd "M-.") nil)))
(setq jedi:use-shortcuts t)

(add-hook 'python-mode-hook #'smartparens-mode)
