;; portable setting
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-prefer-newer t)
(setq custom-file (locate-user-emacs-file "custom.el"))

;; package.el setting
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
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
(el-get-bundle yasnippet-snippets)
(el-get-bundle helm-c-yasnippet)

;; Company
(el-get-bundle company-mode)
(el-get-bundle company-quickhelp)
(el-get-bundle helm-company)

;; Auto-Complete
;; Basically I prefer Company but some Languages doesn't have Company backend but Auto-Complete one.
(el-get-bundle auto-complete)

;; flycheck
(el-get-bundle flycheck)
(el-get-bundle flycheck-pos-tip)

;; projectile
(el-get-bundle projectile)
(el-get-bundle helm-projectile)

;; UI
(el-get-bundle font-lock+ :type emacswiki)
(el-get-bundle all-the-icons)
(el-get-bundle all-the-icons-dired :type git :url "https://github.com/jtbm37/all-the-icons-dired.git" :depends all-the-icons)
(el-get-bundle spaceline)
(el-get-bundle spaceline-all-the-icons :type git :url "https://github.com/domtronn/spaceline-all-the-icons.el.git" :depends (all-the-icons spaceline memoize))

;; util
(el-get-bundle use-package)
(el-get-bundle smartrep)
(el-get-bundle anzu)
(el-get-bundle zlc)
(el-get-bundle switch-buffer-functions)
(el-get-bundle exec-path-from-shell)
(el-get-bundle expand-region)
(el-get-bundle backward-forward)
(el-get-bundle sudo-edit)
(el-get-bundle dired-quick-sort)
(el-get-bundle auto-compile)
(el-get-bundle super-save :type git :url "https://github.com/bbatsov/super-save.git")
(el-get-bundle flyspell-correct)
(el-get-bundle flyspell-correct-helm :type git :url "https://github.com/d12frosted/flyspell-correct.git" :depends (flyspell-correct helm))
(el-get-bundle ddskk)
(el-get-bundle image+)
(el-get-bundle google-this)
(el-get-bundle google-translate)
(el-get-bundle smartparens)
(el-get-bundle migemo)
(el-get-bundle ripgrep :type git :url "https://github.com/nlamirault/ripgrep.el.git")
(el-get-bundle resize-window :type git :url "https://github.com/dpsutton/resize-window.git")
(el-get-bundle region-bindings-mode)
(el-get-bundle quickrun)
(el-get-bundle popwin)
(el-get-bundle open-junk-file)
(el-get-bundle neotree)
(el-get-bundle init-loader)
(el-get-bundle imenu-list :type git :url "https://github.com/bmag/imenu-list.git")
(el-get-bundle highlight-symbol)
(el-get-bundle diminish)
(if (eq system-type 'windows-nt)
    (el-get-bundle magit
      :info nil)                        ;windows fix
  (el-get-bundle magit))
(el-get-bundle evil-mc :type git :url "https://github.com/gabesoft/evil-mc.git" :depends (evil))
(el-get-bundle visual-regexp-steroids)
(el-get-bundle names :type git :url "https://github.com/Malabarba/names.git")
(el-get-bundle electric-operator :type git :url "https://github.com/davidshepherd7/electric-operator.git")
(el-get-bundle recentf-ext)
(el-get-bundle rainbow-delimiters)
(el-get-bundle delight)
(el-get-bundle csv-mode)

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

;; Python
(el-get-bundle elpy)
(el-get-bundle cython-mode)
(el-get-bundle flycheck-cython :type git :url "https://github.com/lbolla/emacs-flycheck-cython.git" :depends flycheck)

;; Julia
(el-get-bundle julia-mode :type elpa)
(el-get-bundle ess :type elpa)

;; JS
(el-get-bundle js2-mode)

;; TeX
(el-get-bundle auctex)

;; Kotlin
(el-get-bundle kotlin-mode)
(el-get-bundle flycheck-kotlin :type git :url "https://github.com/whirm/flycheck-kotlin.git" :depends flycheck)

;; Common Lisp
(el-get-bundle slime)
(el-get-bundle slime-company)

;; Rust
(el-get-bundle rust-mode)
(el-get-bundle emacs-racer :depends (f))
(el-get-bundle flycheck-rust)

;; other modes
(el-get-bundle meghanada :type git :url "https://github.com/mopemope/meghanada-emacs.git" :depends (yasnippet company-mode flycheck))
(el-get-bundle powershell)
(el-get-bundle yaml-mode)
(el-get-bundle tuareg-mode)
(el-get-bundle sql-indent)
(el-get-bundle vimrc-mode)
(el-get-bundle plantuml-mode)
(el-get-bundle markdown-mode)
(el-get-bundle matlab-mode :type elpa)
(el-get-bundle graphviz-dot-mode)
(el-get-bundle pandoc-mode)
(el-get-bundle tablist :type elpa)
(el-get-bundle pdf-tools :type elpa)

;; theme
(el-get-bundle color-theme-zenburn)

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))
(el-get 'sync)

;; load settings
(require 'use-package)
(use-package init-loader
  :config
  (init-loader-load (locate-user-emacs-file "inits")))
