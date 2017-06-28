(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

;;; virtualenv
(require 'virtualenvwrapper)
;; (require 'auto-virtualenvwrapper)
;; (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
;; (add-hook 'projectile-after-switch-project-hook #'auto-virtualenvwrapper-activate)

;;; IDEてきなやついろいろ
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook #'smartparens-mode)

(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=200"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(require 'jedi)
;;; jediの補完はPYTHONPATHを見てくれるらしい
(setq jedi:complete-on-dot t)
(define-key python-mode-map (kbd "C-M-i") 'jedi:complete)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq ac-sources
                  (delete 'ac-source-words-in-same-mode-buffers ac-sources))))

;; jediのdocstring参照をpopwinで出すようにする
(require 'popwin)
(push '("*jedi:doc*" :position bottom :width 30)
      popwin:special-display-config)

;;; evil fix
(require 'evil)
(add-hook 'python-mode-hook
          (lambda ()
            (evil-make-intercept-map jedi-mode-map)
            ;; (setenv "PATH" (concat (getenv "PYTHONPATH") ":" (getenv "PATH")))
            ;; (exec-path-from-shell-copy-envs '("PATH" "VIRTUALENVWRAPPER_PYTHON" "WORKON_HOME" "PYTHONPATH"))
            ))
(setq jedi:use-shortcuts t)
