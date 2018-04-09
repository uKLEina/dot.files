;; portable setting
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-prefer-newer t)
(setq custom-file (locate-user-emacs-file "custom.el"))

;; package.el setting
(require 'package)
(setq package-archive-priorities
      '(("melpa-stable" . 30)
        ("gnu" . 10)
        ("melpa" . 0)))
(package-initialize)

;;; initial el-get setting
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; packages to install
;; evil
(el-get-bundle evil)
(el-get-bundle paredit)
(el-get-bundle evil-paredit :type git :url "https://github.com/roman/evil-paredit.git" :depends (evil paredit))

;; helm
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-swoop)
(el-get-bundle smex)
(el-get-bundle helm-smex :type git :url "https://github.com/ptrv/helm-smex.git" :depends (helm smex))
(el-get-bundle helm-themes)
(el-get-bundle helm-descbinds)

;; yasnippet
(el-get-bundle yasnippet)
(el-get-bundle helm-c-yasnippet)

;; AC/Company
(el-get-bundle auto-complete)
(el-get-bundle ac-dabbrev)
(el-get-bundle ac-helm)
(el-get-bundle ac-octave)
(el-get-bundle ac-ispell :type git :url "https://github.com/syohex/emacs-ac-ispell.git" :depends auto-complete)
(el-get-bundle company-mode)
(el-get-bundle company-quickhelp)
(el-get-bundle helm-company)

;; flycheck
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)

;; projectile
(el-get-bundle projectile)
(el-get-bundle helm-projectile)

;; UI
(el-get-bundle all-the-icons)
(el-get-bundle all-the-icons-dired :type git :url "https://github.com/jtbm37/all-the-icons-dired.git" :depends all-the-icons)
(el-get-bundle spaceline)
(el-get-bundle spaceline-all-the-icons :type git :url "https://github.com/domtronn/spaceline-all-the-icons.el.git" :depends (all-the-icons spaceline memoize))

;; util
(el-get-bundle use-package)
(el-get-bundle smartrep)
(el-get-bundle anzu)
(el-get-bundle end-mark :type http :url "https://raw.githubusercontent.com/tarao/elisp/master/end-mark.el")
(el-get-bundle zlc)
(el-get-bundle switch-buffer-functions)
(el-get-bundle exec-path-from-shell)
(el-get-bundle expand-region)
(el-get-bundle backward-forward)
(el-get-bundle sudo-edit)
(el-get-bundle dired-quick-sort)
(el-get-bundle auto-compile)
(el-get-bundle auto-save)
(el-get-bundle flyspell-correct)
(el-get-bundle flyspell-correct-helm :type git :url "https://github.com/d12frosted/flyspell-correct.git" :depends (flyspell-correct helm))
;; (el-get-bundle git-commit-mode)
(el-get-bundle ddskk)
(el-get-bundle image+)
(el-get-bundle google-this)
(el-get-bundle google-translate)
(el-get-bundle smartparens)
(el-get-bundle migemo)
(el-get-bundle ripgrep)
(el-get-bundle resize-window)
(el-get-bundle region-bindings-mode)
(el-get-bundle quickrun)
(el-get-bundle popwin)
(el-get-bundle open-junk-file)
(el-get-bundle neotree)
(el-get-bundle init-loader)
(el-get-bundle imenu-list)
(el-get-bundle highlight-symbol)
(el-get-bundle diminish)
(if (eq system-type 'windows-nt)
    (el-get-bundle magit
      :info nil)                        ;windows fix
  (el-get-bundle magit))
(el-get-bundle evil-mc :type git :url "https://github.com/gabesoft/evil-mc.git" :depends (evil))
(el-get-bundle visual-regexp-steroids)


;; C/C++
(el-get-bundle irony-mode)
(el-get-bundle irony-eldoc)
(el-get-bundle company-irony)
(el-get-bundle company-irony-c-headers :type git :url "https://github.com/hotpxl/company-irony-c-headers.git")
(el-get-bundle flycheck-irony)
(el-get-bundle clang-format)
(el-get-bundle cmake-mode)

;; PHP
(el-get-bundle php-mode)
                                        ;(el-get-bundle geben)
(el-get-bundle ac-php)

;; Python
(el-get-bundle elpy)
(el-get-bundle cython-mode)
(el-get-bundle flycheck-cython :type git :url "https://github.com/lbolla/emacs-flycheck-cython.git" :depends flycheck)

;; JS
(el-get-bundle js2-mode)
(el-get-bundle ac-js2)

;; other modes
(el-get-bundle meghanada :type git :url "https://github.com/mopemope/meghanada-emacs.git" :depends (yasnippet company-mode flycheck))
(el-get-bundle powershell)
(el-get-bundle yaml-mode)
(el-get-bundle tuareg-mode)
(el-get-bundle sql-indent)
(el-get-bundle vimrc-mode)
(el-get-bundle plantuml-mode)
(el-get-bundle markdown-mode)

;; theme
(el-get-bundle color-theme-zenburn)

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))
(el-get 'sync)

;; load settings
(require 'use-package)
(use-package init-loader
  :config
  (init-loader-load (locate-user-emacs-file "inits")))
