;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-prefer-newer t)
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 30)
        ("melpa" . 20)
        ("gnu" . 10)))
(package-initialize)

(setq package-pinned-packages
      '((all-the-icons . "melpa")
        (highlight-symbol . "melpa")
        (esup . "melpa")
        (direx . "melpa")
        (multi-term . "melpa")))

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-l"))
(bind-keys ("C-l C-l" . recenter-top-bottom)
           ("C-l C-x" . server-edit)
           ("C-l C-<tab>" . tab-to-tab-stop))

;;; customize
(set-language-environment "Japanese")
(auto-compression-mode +1)
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

(savehist-mode 1)
(setq-default save-place t)
(use-package saveplace)
(display-time)
(line-number-mode 1)
(column-number-mode 1)
(setq gc-cons-threshold (* 100 gc-cons-threshold))
(setq gc-cons-percentage 0.2)
(setq message-log-max 100000)
(setq enable-recursive-minibuffers t)
(setq use-dialog-box nil)
(defalias 'message-box 'message)
(setq history-length 10000)
(setq echo-keystrokes 0.1)
(setq large-file-warning-threshold (* 25 1024 1024))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvar default-tab-width 4)
(setq scroll-step 1)
(setq initial-scratch-message "")
(setq delete-auto-save-files t)
;; show filename and path in title bar
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))
(display-time-mode +1)
(setq require-final-newline t)
;; デフォルト色付け
(use-package generic-x
  :defer t)
(show-paren-mode 1)
;; hide tool bar/scroll bar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(if window-system (add-to-list 'default-frame-alist '(alpha . 90)))

(recentf-mode 1)

;;; delete path hierarchy by hierarchy in minibuffer by M-h
;;; tips; M-h works as "mark-paragraph" in a main buffer.
(defun my-minibuffer-delete-parent-directory ()
  "Delete one level of file path."
  (interactive)
  (let ((current-pt (point)))
    (when (re-search-backward "/[^/]+/?" nil t)
      (forward-char 1)
      (delete-region (point) current-pt))))
(define-key minibuffer-local-map (kbd "M-h") 'my-minibuffer-delete-parent-directory)

;; バッファの開始・終端を明示する
(setq-default indicate-buffer-boundaries 'left)

;;; visible spaces
(defface f-bg-aquamarine '((t (:background "medium aquamarine"))) nil)
(defface f-bg-gray '((t (:background "gray"))) nil)
(defface f-ub-steelblue '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar f-bg-aquamarine 'f-bg-aquamarine)
(defvar f-bg-gray 'f-bg-gray)
(defvar f-ub-steelblue 'f-ub-steelblue)
(defun visible-spaces (&rest args)
  (font-lock-add-keywords
   major-mode
   '(("　" 0 f-bg-aquamarine append)
     ("\t" 0 f-ub-steelblue prepend)
     ("[ ]+$" 0 f-bg-gray append))))
(advice-add 'font-lock-mode :before #'visible-spaces)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; バックアップファイルはうっとおしいので一箇所にまとめてしまう
(custom-set-variables
 '(backup-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/backup"))))
 '(auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backup") t))))
;; バックアップまでの間隔も短くする
(setq auto-save-timeout 15)
(setq auto-save-interval 60)

;; window
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;;; scroll window without changing cursor position
(defun my-count-string-columns (str)
  "Count columns of string. The number of column begins 0."
  (with-temp-buffer
    (insert str)
    (current-column)))

(defun my-count-lines-window ()
  "Count lines relative to the selected window. The number of lines begins 0."
  (interactive)
  (let* ((window-string (buffer-substring-no-properties (window-start) (point)))
         (line-string-list (split-string window-string "\n"))
         (line-count 0)
         line-count-list)
    (setq line-count (1- (length line-string-list)))
    (unless truncate-lines      ; consider folding back
      ;; `line-count-list' is list of the number of physical lines which each logical line has.
      (setq line-count-list (mapcar #'(lambda (str)
                                        (/ (my-count-string-columns str) (window-width)))
                                    line-string-list))
      (setq line-count (+ line-count (apply '+ line-count-list))))
    line-count))

(defadvice scroll-up (around scroll-up-relative activate)
  "Scroll up relatively without move of cursor."
  (let ((line (my-count-lines-window)))
    ad-do-it
    (move-to-window-line line)))

(defadvice scroll-down (around scroll-down-relative activate)
  "Scroll down relatively without move of cursor."
  (let ((line (my-count-lines-window)))
    ad-do-it
    (move-to-window-line line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package auto-compile
  :ensure t
  :defer t
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package super-save
  :ensure t
  :custom
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t)
  (custom-set-variables '(window-divider-default-right-width 10))
  (window-divider-mode +1))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :commands (doom-modeline-def-modeline doom-modeline-def-segment)
  :init
  (defun remove-padding-zero (num)
    (if (string= (substring num 0 1) "0")
        (substring num 1)
      num))

  (doom-modeline-def-segment buffer-size
    "Display current buffer size"
    (format-mode-line " %IB"))

  (doom-modeline-def-segment projectile-project-name
    "Display Projectile project name"
    (if (and (boundp 'projectile-mode) projectile-mode)
        (propertize (format " Project: %s" (projectile-default-project-name (projectile-project-root)))
                    'face `(:foreground "#8cd0d3" :weight bold))
      ""))

  (doom-modeline-def-segment linum-colnum
    "Display current linum/colnum"
    (propertize (format " Ln %s/%s, Col %s" (format-mode-line "%l") (line-number-at-pos (point-max)) (format-mode-line "%c"))
                'face `(:foreground "#8cd0d3" :weight bold)))

  (doom-modeline-def-segment datetime
    "Display datetime on modeline"
    (let* ((system-time-locale "C")
           (dow (format "%s" (format-time-string "%a")))
           (month (format "%s" (remove-padding-zero (format-time-string "%m")) ))
           (day (format "%s" (remove-padding-zero (format-time-string "%d"))))
           (hour (format "%s" (remove-padding-zero (format-time-string "%I"))))
           (minute (format-time-string "%M"))
           (am-pm (format-time-string "%p")))
      (propertize
       (concat
        " "
        hour
        ":"
        minute
        am-pm
        "  "
        )
       'help-echo "Show calendar"
       'mouse-face '(:box 1)
       'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (calendar))))))

  (doom-modeline-def-segment python-venv
    "Display current python venv name"
    (if (string= major-mode "python-mode")
        (let ((venv-name (if (or (not (boundp 'pyvenv-virtual-env-name))
                                 (eq pyvenv-virtual-env-name nil))
                             "GLOBAL"
                           pyvenv-virtual-env-name)))
          (propertize (format " [%s]" venv-name) 'face '(:foreground "#f0dfaf" :weight bold)))
      ""))

  (doom-modeline-def-modeline 'simple
    '(bar evil-state matches remote-host buffer-info  pdf-pages linum-colnum)
    '(projectile-project-name python-venv vcs checker fancy-battery datetime))
  (doom-modeline-def-modeline 'verbose
    '(bar matches remote-host buffer-info-simple buffer-size)
    '(major-mode minor-modes buffer-encoding))

  (defun setup-initial-doom-modeline ()
    (doom-modeline-set-modeline 'simple t))
  (add-hook 'doom-modeline-mode-hook 'setup-initial-doom-modeline)

  (defvar doom-modeline-simple-p t)
  (defun switch-modeline ()
    (interactive)
    (if doom-modeline-simple-p
        (doom-modeline-set-modeline 'verbose)
      (doom-modeline-set-modeline 'simple))
    (force-mode-line-update)
    (setq doom-modeline-simple-p (not doom-modeline-simple-p)))
  (bind-key "C-l C-m" 'switch-modeline)
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-checker-simple-format nil)
  (setq doom-modeline-bar-width 10)
  ;; mode-line color as evil state
  ;; normal: BG-Alt
  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (set-face-background 'mode-line (doom-color 'bg-alt))))
  ;; insert: Green
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (set-face-background 'mode-line (doom-darken (doom-color 'green) 0.5))))
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (set-face-background 'mode-line (doom-color 'bg-alt))))
  ;; visual: Blue
  (add-hook 'evil-visual-state-entry-hook
            (lambda ()
              (set-face-background 'mode-line (doom-color 'dark-blue))))
  (add-hook 'evil-visual-state-exit-hook
            (lambda ()
              (set-face-background 'mode-line (doom-color 'bg-alt))))
  ;; emacs: Red
  (add-hook 'evil-emacs-state-entry-hook
            (lambda ()
              (set-face-background 'mode-line (doom-color 'magenta))))
  (add-hook 'evil-emacs-state-exit-hook
            (lambda ()
              (set-face-background 'mode-line (doom-color 'bg-alt)))))

(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  :init
  ;; Evil compatibility fix
  (defun evil-end-of-line-p ()
    (let ((offset (if (evil-emacs-state-p)
                      0
                    1)))
      (eq (- (line-end-position) offset)  (point))))

  (defun evil-beginning-of-line-p ()
    (eq (line-beginning-position) (point)))

  (defun evil-paredit-forward (origfun arg)
    (funcall origfun)
    (unless (evil-emacs-state-p)
      (backward-char)))

  (defun evil-paredit-backward (origfun arg)
    (if (evil-end-of-line-p)
        (forward-line)
      (forward-char))
    (funcall origfun))

  (advice-add 'paredit-forward :around #'evil-paredit-forward)
  (advice-add 'paredit-backward :around #'evil-paredit-backward))

(use-package posframe
  :ensure t
  :defer t)

(use-package evil
  :ensure t
  :custom
  (evil-echo-state nil)
  :config
  (evil-mode 1)
  (bind-keys :map evil-normal-state-map
             ("M-." . xref-find-definitions)
             ("J" . (lambda ()
                      (interactive)
                      (evil-scroll-line-down 1)))
             ("K" . (lambda ()
                      (interactive)
                      (evil-scroll-line-up 1)))
             ("C-e" . end-of-line)
             ("C-t" . other-window-or-split)
             :map evil-insert-state-map
             ("C-t" . other-window-or-split)
             ("C-e" . end-of-line)))

(use-package which-key
  :ensure t
  :hook
  (after-init . which-key-mode))

(use-package migemo
  :ensure t
  :defer t
  :commands (migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary (expand-file-name "/usr/share/cmigemo/utf-8/migemo-dict"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-pattern-alist-length 1024)
  (migemo-coding-system 'utf-8)
  :init
  (migemo-init))

(use-package ripgrep
  :ensure t
  :defer t
  :custom
  (ripgrep-executable "~/.cargo/bin/rg")
  (ripgrep-arguments '("-S")))

(use-package popwin
  :ensure t
  :defer t
  :commands (popwin-mode))

(use-package helm
  :ensure t
  :init
  (helm-mode +1)
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-for-files)
   ("M-y" . helm-show-kill-ring)
   :map helm-map ("C-h" . delete-backward-char))
  :custom
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  :config
  (use-package helm-config)
  (use-package helm-descbinds
    :ensure t
    :config
    (helm-descbinds-mode +1))
  (helm-autoresize-mode +1)
  (with-eval-after-load 'migemo
    (helm-migemo-mode +1)))

;; (defun helm-find-files-all-the-icons (arg)
;;   (let ((disp (car arg))
;;         (file (cdr arg)))
;;     (cons (format "%s\t%s" (all-the-icons-icon-for-file disp) disp) file)))
;; (advice-add 'helm-ff-filter-candidate-one-by-one :filter-return #'helm-find-files-all-the-icons)

(use-package smex :ensure t)
(use-package helm-smex
  :ensure t
  :bind (("M-X" . helm-smex-major-mode-commands))
  :init
  (global-set-key [remap execute-extended-command] #'helm-smex)
  :custom
  (helm-smex-show-bindings t))

(use-package helm-ag
  :ensure t
  :defer t
  :custom
  (helm-ag-base-command "rg --vimgrep --no-heading")
  (helm-ag-insert-at-point 'symbol)
  :bind
  (("C-M-S-g" . 'helm-ag))
  :config
  (defun helm-projectile-ag ()
    "Projectileと連携"
    (interactive)
    (helm-ag (projectile-project-root))))

(use-package helm-rg
  :ensure t
  :bind
  (("C-M-g" . 'helm-rg)))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line)
         ("M-i" . helm-multi-swoop-all-from-helm-swoop))
  :custom
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-split-direction 'split-window-vertically)
  (helm-swoop-speed-or-color t)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-line-number-face t))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :config
  (bind-keys
   :map company-active-map
   ("C-s" . company-filter-candidates)
   ("C-h" . delete-backward-char)
   :map company-mode-map
   ("C-M-i" . company-complete)))

(use-package company-quickhelp
  :ensure t
  :hook
  (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.2))

(use-package smartrep
  :ensure t
  :config
  (smartrep-define-key
      global-map "C-x"
    '(("}" . enlarge-window-horizontally)
      ("{" . shrink-window-horizontally)
      ("o" . other-window)))
  (smartrep-define-key
      global-map "C-l"
    '(("<tab>" . tab-to-tab-stop))))

(use-package popwin
  :config
  (popwin-mode +1))

(use-package xref
  :defer t
  :init
  (push '("*xref*" :position bottom :width 5)
        popwin:special-display-config))

(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-add-next-checker)
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook
  (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 2)
  :config
  (use-package flycheck-pos-tip :ensure t)
  (flycheck-pos-tip-mode +1)
  (smartrep-define-key
      flycheck-mode-map "C-c !"
    '(("n" . flycheck-next-error)
      ("p" . flycheck-previous-error)))
  (push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
        popwin:special-display-config))

(use-package elpy
  :ensure t
  :defer t
  :init
  ;; never use `elpy-enable' because it slows Emacs startup time :(
  ;; so manually enable all elpy features
  (add-hook 'python-mode-hook 'elpy-modules-global-init)
  (add-hook 'python-mode-hook 'elpy-mode)
  (add-hook 'pyvenv-post-activate-hooks 'elpy-rpc--disconnect)
  (add-hook 'inferior-python-mode-hook 'elpy-shell--enable-output-filter)
  :custom
  ;; remove flymake from modules
  (elpy-modules '(elpy-module-sane-defaults
                  elpy-module-company
                  elpy-module-eldoc
                  elpy-module-highlight-indentation
                  elpy-module-pyvenv
                  elpy-module-yasnippet
                  elpy-module-django))
  (python-shell-interpreter "jupyter")
  (python-shell-interpreter-args "console --simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  ;; (elpy-shell-display-buffer-after-send t)
  :config
  (define-key inferior-python-mode-map (kbd "C-c C-z") 'elpy-shell-switch-to-buffer)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (bind-key "C-l C-v" #'pyvenv-workon elpy-mode)
  (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)

  ;; use both flake8 and pylint
  ;; flycheck uses only flake8 by default,
  ;; so add pylint after it
  (when (eq system-type 'gnu/linux)
    (flycheck-add-next-checker 'python-flake8 'python-pylint))

  ;; popwin
  (push '("*Python Doc*" :position bottom :width 30 :noselect t)
        popwin:special-display-config)
  (push '(inferior-python-mode :position bottom :width 30)
        popwin:special-display-config))

;;; dired
(use-package dired
  :defer t
  :custom
  ;; fix keybind for SKK
  (dired-bind-jump nil)
  :config
  (bind-keys :map dired-mode-map
             ("C-t" . other-window-or-split)
             )
  (use-package dired-x)
  (custom-set-variables '(dired-omit-files (concat dired-omit-files "\\|^\\..+$")))
  (bind-key "C-l C-o" 'dired-omit-mode dired-mode-map)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
  (when (eq system-type 'gnu/linux)
    (custom-set-variables '(dired-listing-switches "-AFDlh --group-directories-first"))
    ;; ファイルのあるディレクトリを起点に Nautilus を開く
    (defun file-open-nautilus ()
      (interactive)
      (call-process "nautilus" nil nil nil "--no-desktop" "-n"
                    (or (file-name-directory buffer-file-name)
                        default-directory)))
    (bind-key "C-l C-e" 'file-open-nautilus)

    ;; http://qiita.com/items/2620874c802db60c99f9
    (defun dired-open-nautilus ()
      (interactive)
      (call-process "nautilus" nil 0 nil (dired-current-directory)))
    (bind-key "e" 'dired-open-nautilus dired-mode-map))
  (when (eq system-type 'windows-nt)
    (custom-set-variables '(ls-lisp-dirs-first t)))
  (use-package dired-quick-sort
    :ensure t
    :commands (hydra-dired-quick-sort/body)
    :init
    (bind-key "S" 'hydra-dired-quick-sort/body dired-mode-map)))

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package direx
  :ensure t
  :bind
  (("<f8>" . direx:jump-to-directory-other-window))
  :config
  (push '(direx:direx-mode :position left :width 40 :dedicated t)
        popwin:special-display-config)
  (evil-define-key 'normal direx:direx-mode-map (kbd "j") 'direx:next-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "k") 'direx:previous-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "J") 'direx:next-sibling-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "K") 'direx:previous-sibling-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "^") 'direx:up-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "RET") 'direx:maybe-find-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "TAB") 'direx:toggle-item))

(use-package recentf-ext :ensure t)

(use-package skk
  :ensure ddskk
  :bind
  (("C-x j" . skk-auto-fill-mode)
   ("C-x C-j" . skk-mode))
  :custom
  (skk-user-directory "~/.skk.d")
  (skk-dcomp-activate t)
  (skk-show-candidates-always-pop-to-buffer t)
  :config
  (use-package skk-hint)
  (use-package skk-study))

(use-package image
  :defer t
  :config
  (use-package image+
    :ensure t
    :config
    (defhydra imagex-sticky-binding (global-map "C-l i")
      "Manipulating Image"
      ("+" imagex-sticky-zoom-in "zoom in")
      ("-" imagex-sticky-zoom-out "zoom out")
      ("M" imagex-sticky-maximize "maximize")
      ("O" imagex-sticky-restore-original "restore original")
      ("S" imagex-sticky-save-image "save file")
      ("r" imagex-sticky-rotate-right "rotate right")
      ("l" imagex-sticky-rotate-left "rotate left"))))

(use-package imenu-list
  :ensure t
  :defer t
  :custom
  (imenu-list-auto-resize t)
  :bind
  ("C-;" . imenu-list-smart-toggle))

(use-package winner
  :init
  (winner-mode +1)
  :config
  (defun winner-dwim (arg)
    (interactive "p")
    (let ((func (pcase arg
                  (4 'winner-redo)
                  (1 'winner-undo))))
      (call-interactively func)
      (run-with-timer 0.01 nil 'set 'last-command func)))
  :bind
  (("C-q" . winner-dwim)
   ("C-l C-q" . quoted-insert)))

(use-package magit
  :ensure t
  :bind (("C-l m s" . magit-status)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-use-migemo t)
  (anzu-search-threshold 1000)
  :bind
  (("C-M-%" . anzu-query-replace-at-cursor)         ; replace currnet string in entire buffer with query
   ("C-M-#" . anzu-query-replace-at-cursor-thing)   ; replace currnet string only in cursor thing(function etc.)
   ))

(use-package backward-forward
  :ensure t
  :init
  (backward-forward-mode +1)
  :config
  (setq backward-forward-evil-compatibility-mode t)
  (advice-add 'evil-goto-first-line :before #'backward-forward-push-mark-wrapper)
  (advice-add 'evil-goto-line :before #'backward-forward-push-mark-wrapper)
  :bind
  (:map backward-forward-mode-map
        ("C-l C-a" . backward-forward-previous-location)
        ("C-l C-f" . backward-forward-next-location)))

(use-package electric-operator
  :ensure t
  :commands (electric-operator-add-rules-for-mode electric-operator-get-rules-for-mode)
  :hook
  ((python-mode . electric-operator-mode)))

(use-package expand-region
  :ensure t
  :bind
  (("C-M-]" . er/expand-region)
   ("C-M-:" . er/contract-region)))

(use-package hideshow
  :hook
  ((c-mode-common emacs-lisp-mode java-mode lisp-mode perl-mode sh-mode python-mode) . hs-minor-mode)
  :bind
  (:map hs-minor-mode-map
        ("C-l h" . hs-toggle-hiding)))

(use-package highlight-symbol
  :ensure t
  :hook
  ((prog-mode . highlight-symbol-mode)
   (prog-mode . highlight-symbol-nav-mode))
  :custom
  (highlight-symbol-idle-delay 0.5)
  (highlight-symbol-occurrence-message '(explicit))
  :custom-face
  ;; auto highlight darker for Doom Dracula theme
  (highlight-symbol-face ((t (:background "#1b1d26"))))
  :bind
  ("C-l C-s" . highlight-symbol))

(use-package smartparens
  :ensure t
  :hook
  (prog-mode . smartparens-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(use-package sudo-edit
  :ensure t
  :defer t)

(use-package google-translate
  :ensure t
  :defer t
  :commands (google-translate-translate)
  :init
  (defvar google-translate-english-chars "[:ascii:]"
    "これらの文字が含まれているときは英語とみなす")
  (defun google-translate-enja-or-jaen (&optional string)
    "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (thing-at-point 'word))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))
  (bind-key "C-l C-t" 'google-translate-enja-or-jaen)
  :config
  (use-package google-translate-smooth-ui)
  (push '("*Google Translate*" :position bottom :width 30)
        popwin:special-display-config))

(use-package visual-regexp-steroids
  :ensure t
  :bind
  (("M-%" . vr/query-replace)))

(use-package google-this
  :ensure t
  :bind
  (("C-l g" . google-this)))

(use-package projectile
  :ensure t
  :commands (projectile-project-root)
  :bind (("C-l p e" . projectile-mode)))

(use-package helm-projectile
  :ensure t
  :bind (("C-l p f " . helm-projectile-find-file-dwim)))

(use-package yasnippet
  :ensure t
  :commands (yas-expand)
  :hook
  (prog-mode . yas-minor-mode)
  :bind (("C-<tab>" . yas-expand))
  :config
  (use-package yasnippet-snippets :ensure t))

(use-package ivy-yasnippet
  :ensure t
  :bind (("C-l y" . ivy-yasnippet)))

(use-package realgud
  :ensure t
  :defer t)

;;; C
(defun c/c++-mode-setup ()
  "Hook function to setup `c-mode' and `c++-mode'."
  (projectile-mode)
  (hs-minor-mode 1))
(add-hook 'c-mode-hook #'c/c++-mode-setup)
(add-hook 'c++-mode-hook #'c/c++-mode-setup)

(use-package rtags
  :load-path "/usr/local/share/emacs/site-lisp/rtags/"
  :init
  (defun c/c++-mode-rtags-setup ()
    "hook function to setup rtags things for `c-mode' and `c++-mode'."
    (when (rtags-is-indexed)
      (rtags-start-process-unless-running)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
      (local-set-key (kbd "M-,") 'rtags-location-stack-back)
      (local-set-key (kbd "M-@") 'rtags-find-references)
      (local-set-key (kbd "C-M-.") 'rtags-next-match)
      (local-set-key (kbd "C-M-,") 'rtags-next-match)
      (custom-set-variables
       '(rtags-completions-enabled t)
       '(rtags-autostart-diagnostics t))
      (use-package company-rtags)
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-rtags))
      (use-package flycheck-rtags)
      (flycheck-select-checker 'rtags)
      (push '("*RTags*" :position bottom :noselect t)
            popwin:special-display-config)))
  (add-hook 'c-mode-hook #'c/c++-mode-rtags-setup)
  (add-hook 'c++-mode-hook #'c/c++-mode-rtags-setup)
  :config
  (evil-make-overriding-map c++-mode-map)
  (evil-make-overriding-map c-mode-map))

(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (use-package company-irony :ensure t)
  (company-irony-setup-begin-commands)
  (use-package company-irony-c-headers :ensure t)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  (use-package flycheck-irony :ensure t)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package clang-format
  :ensure t
  :defer t
  :init
  (bind-key "C-l i" 'clang-format-buffer c++-mode-map)
  (bind-key "C-l i" 'clang-format-buffer c-mode-map))

(use-package hideif
  :defer t
  :init
  (add-hook 'c++-mode-hook 'hide-ifdef-mode)
  (add-hook 'c-mode-hook 'hide-ifdef-mode))

(use-package js2-mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . js2-mode)))

(use-package slime
  :ensure t
  :mode
  (("\\.lisp\\'" . lisp-mode))
  :hook
  (lisp-mode . slime-mode)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (use-package slime-company :ensure t)
  (slime-setup '(slime-repl slime-fancy slime-company)))

(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)
         ("\\.pxi\\'" . cython-mode))
  :config
  (use-package flycheck-cython
    :ensure t
    :hook
    (cython-mode . flycheck-mode)))

(use-package graphviz-dot-mode
  :ensure t
  :mode
  (("\\.dot\\'" . graphviz-dot-mode)
   ("\\.gc\\'" . graphviz-dot-mode)))

(use-package ess
  :ensure t
  :mode (("\\.jl\\'" . ess-julia-mode))
  :config
  (apply #'electric-operator-add-rules-for-mode
         'ess-julia-mode
         (electric-operator-get-rules-for-mode 'prog-mode))
  (electric-operator-add-rules-for-mode
   'ess-julia-mode
   (cons "=" #'electric-operator-julia-mode-kwargs-=)
   (cons ";" "; ")

   ;; Subtype comparison
   (cons "<:" " <: ")

   ;; Cool! Unicode!
   (cons "÷" " ÷ ")
   (cons "≠" " ≠ ")
   (cons "≤" " ≤ ")
   (cons "≥" " ≥ ")

   ;; something about fractions
   (cons "//" " // ")
   (cons ".//" " .// ")
   (cons "//=" " //= ")

   ;; pipe
   (cons "|>" " |> ")

   (cons "*" " * ")
   (cons "/" " / ")
   (cons "%" " % ")
   (cons "&" " & ")

   ;; \ (escaped), for solving matrix multiplies
   (cons "\\" " \\ ")
   (cons "\\=" " \\= ")
   (cons ".\\" " .\\ ")

   ;; XOR
   (cons "$" " $ ")

   ;; Even more equal!
   (cons "===" " === ")
   (cons "!==" " !== ")

   ;; vector operations and assign-operators
   (cons ".^" " .^ ")
   (cons ".*" " .* ")
   (cons "./" " ./ ")
   (cons ".%" " .% ")
   (cons "<<" " << ")
   (cons ">>" " >> ")
   (cons ">>>" " >>> ")
   (cons ".<<" " .<< ")
   (cons ".>>" " .>> ")
   (cons ".>>>" " .>>> ")
   (cons ".+" " .+ ")
   (cons ".-" " .- ")
   (cons ".>" " .> ")
   (cons ".<" " .< ")
   (cons ".>=" " .>= ")
   (cons ".<=" " .<= ")
   (cons ".==" " .== ")
   (cons ".!=" " .!= ")
   (cons "^=" " ^= ")
   (cons "÷=" " ÷= ")
   (cons "%=" " %= ")
   (cons "|=" " |= ")
   (cons "&=" " &= ")
   (cons "$=" " $= ")
   (cons "<<=" " <<= ")
   (cons ">>=" " >>= ")
   (cons ">>>=" " >>>= ")
   (cons ".+=" " .+= ")
   (cons ".-=" " .-= ")
   (cons ".*=" " .*= ")
   (cons "./=" " ./= ")
   (cons ".//=" " .//= ")
   (cons ".\\=" " .\\= ")
   (cons ".^=" " .^= ")
   (cons ".÷=" " .÷= ")
   (cons ".%=" " .%= ")))

(use-package matlab
  :ensure matlab-mode
  :mode
  (("\\.m'" . matlab-mode))
  :custom
  (matlab-indent-function-body t))

(use-package tuareg
  :ensure t
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)
         ("\\.mly\\'" . tuareg-mode)
         ("\\.mll\\'" . tuareg-mode)
         ("\\.mlp\\'" . tuareg-mode)))

(use-package pdf-view
  :ensure pdf-tools
  :init
  (pdf-loader-install))

(define-generic-mode 'poe-filter-mode
  ;; Comments
  '("#")
  ;; Blocks
  '("Show" "Hide")
  '(
    ;; Conditions
    ("\\(ItemLevel\\|DropLevel\\|Quality\\|Rarity\\|Class\\|BaseType\\|Sockets\\|LinkedSockets\\|SocketGroup\\|Height\\|Width\\|HasExplicitMod\\|StackSize\\|GemLevel\\|Identified\\|Corrupted\\|ElderItem\\|ShaperItem\\|ElderMap\\|ShapedMap\\|MapTier\\)" . 'font-lock-variable-name-face)
    ;; Actions
    ("\\(SetBorderColor\\|SetTextColor\\|SetBackgroundColor\\|SetFontSize\\|PlayAlertSound\\|PlayAlertSoundPositional\\|DisableDropSound\\|CustomAlertSound\\|MinimapIcon\\|PlayEffect\\)" . 'font-lock-variable-name-face)
    ;; Attributes
    ("\\(True\\|False\\)" . 'font-lock-constant-face)
    ;; Rarity
    ("\\(Unique\\|Rare\\|Magic\\|Normal\\)" . 'font-lock-constant-face)
    ;; Color
    ("\\(Red\\|Green\\|Blue\\|Brown\\|White\\|Yellow\\)" . 'font-lock-constant-face)
    ;; Shape
    ("\\(Circle\\|Diamond\\|Hexagon\\|Square\\|Star\\|Triangle\\)" . font-lock-constant-face)
    ;; Beam
    ("\\(Temp\\)" . font-lock-constant-face)
    ;; Base Type
    ("\\(Jewel\\|Amulets\\|Belt\\|Ring\\|Wands\\|Daggers\\|One Hand\\|Shields\\|Thrusting\\|Sceptre\\|Claws\\|Currency\\|Gems\\|Flask\\|Maps\\|Piece\\)" . font-lock-constant-face)
    ;; Item Size
    ("\\(Small\\|Medium\\|Large\\)" . font-lock-constant-face)
    ;; Flask Tier
    ("\\(Greater\\|Grand\\|Giant\\|Colossal\\|Sacred\\|Hallowed\\|Sanctified\\|Divine\\|Eternal\\)" . font-lock-constant-face)
    )
  '(".filter\\'")
  nil
  "Major mode for editing Path of Exile filter file.")

(use-package powershell
  :ensure t
  :mode (("\\.ps1'" . powershell-mode)))

(use-package plantuml-mode
  :ensure t
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode)
         ("\\.pu\\'" . plantuml-mode))
  :config
  (custom-set-variables '(plantuml-jar-path "~/plantuml.jar"))
  (push '("*PLANTUML Preview*" :position right :width 50 :noselect t)
        popwin:special-display-config))

(use-package rust-mode
  :ensure t
  :defer t
  :hook
  ((rust-mode . smartparens-mode)
   (rust-mode . electric-operator-mode))
  :custom
  (rust-format-on-save t))

(use-package racer
  :ensure t
  :hook
  ((rust-mode . racer-mode-hook)
   (racer-mode . eldoc-mode)
   (racer-mode . company-mode))
  :config
  (bind-key "M-." 'racer-find-definition rust-mode-map))

(use-package flycheck-rust
  :ensure t
  :hook
  ((rust-mode . flycheck-mode)
   (rust-mode . flycheck-rust-setup)))

(use-package tex-jp
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (japanese-TeX-engine-default 'uptex)
  (TeX-engine 'uptex)
  (TeX-PDF-from-DVI "Dvipdfmx")
  (japanese-LaTeX-default-style "bxjsarticle")
  (TeX-view-program-list '(("Evince" "/usr/bin/evince %o")))
  (TeX-view-program-selection '((output-pdf "Evince")))
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  :hook
  ((LaTeX-mode . japanese-latex-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . LaTeX-math-mode))
  :init
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (add-to-list 'TeX-command-list
                                     '("Latexmk"
                                       "latexmk %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-upLaTeX"
                                       "latexmk -e '$latex=q/uplatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-LuaLaTeX"
                                       "latexmk -e '$lualatex=q/lualatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdflua %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Xdg-open"
                                       "xdg-open %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run xdg-open"))
                        (add-to-list 'TeX-command-list
                                     '("Evince"
                                        ;"synctex view -i \"%n:0:%b\" -o %s.pdf -x \"evince -i %%{page+1} %%{output}\""
                                       "TeX-evince-sync-view"
                                       TeX-run-discard-or-function t t :help "Forward search with Evince"))
                        (add-to-list 'TeX-command-list
                                     '("Fwdevince"
                                       "fwdevince %s.pdf %n \"%b\""
                                       TeX-run-discard-or-function t t :help "Forward search with fwdevince"))
                        (add-to-list 'TeX-command-list
                                     '("Atril"
                                        ;"synctex view -i \"%n:0:%b\" -o %s.pdf -x \"atril -i %%{page+1} %%{output}\""
                                       "TeX-atril-sync-view"
                                       TeX-run-discard-or-function t t :help "Forward search with Atril"))
                        (add-to-list 'TeX-command-list
                                     '("Okular"
                                       "okular --unique \"file:\"%s.pdf\"#src:%n %a\""
                                       TeX-run-discard-or-function t t :help "Forward search with Okular"))
                        (add-to-list 'TeX-command-list
                                     '("Zathura"
                                       "zathura -x \"emacsclient --no-wait +%%{line} %%{input}\" --synctex-forward \"%n:0:%b\" %s.pdf"
                                       TeX-run-discard-or-function t t :help "Forward and inverse search with zathura"))
                        (add-to-list 'TeX-command-list
                                     '("Qpdfview"
                                       "qpdfview --unique \"\"%s.pdf\"#src:%b:%n:0\""
                                       TeX-run-discard-or-function t t :help "Forward search with qpdfview"))
                        (add-to-list 'TeX-command-list
                                     '("TeXworks"
                                       "synctex view -i \"%n:0:%b\" -o %s.pdf -x \"texworks --position=%%{page+1} %%{output}\""
                                       TeX-run-discard-or-function t t :help "Forward search with TeXworks"))
                        (add-to-list 'TeX-command-list
                                     '("TeXstudio"
                                       "synctex view -i \"%n:0:%b\" -o %s.pdf -x \"texstudio --pdf-viewer-only --page %%{page+1} %%{output}\""
                                       TeX-run-discard-or-function t t :help "Forward search with TeXstudio"))
                        (add-to-list 'TeX-command-list
                                     '("MuPDF"
                                       "mupdf %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run MuPDF"))
                        (add-to-list 'TeX-command-list
                                     '("Firefox"
                                       "firefox -new-window %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Mozilla Firefox"))
                        (add-to-list 'TeX-command-list
                                     '("Chromium"
                                       "chromium --new-window %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Chromium"))
                        (add-to-list 'TeX-command-list
                                     '("AcroRead"
                                       "wine cmd /c start AcroRd32.exe %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Adobe Acrobat Reader DC")))))
  :config
  (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
    (delq (assoc command TeX-command-list) TeX-command-list))
  (use-package reftex-aux
    :hook
    (LaTeX-mode . turn-on-reftex)
    :custom
    (reftex-plug-into-AUCTeX t))
  )

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :init
  ;; reftex with org mode
  (add-hook 'org-mode-hook 'turn-on-reftex)
  :config
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c [") 'reftex-citation))
  ;; Org Mode LaTeX Export
  (use-package ox-latex)
  (use-package ox-beamer)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-latex-default-class "jsarticle")
  ;; org-latex-classes
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[12pt,a4j,uplatex]{jsarticle}
                 [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 ))
  (custom-set-variables '(org-latex-classes (delete (assoc "beamer" org-latex-classes) org-latex-classes)))
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[dvipdfmx,presentation]{beamer}
                 [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))

(use-package vimrc-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package diminish
  :ensure t
  :config
  (diminish 'flycheck-mode "FlyC")
  (diminish 'highlight-symbol-mode "HighSym")
  (diminish 'smartparens-mode "SmPar")
  (diminish 'hs-minor-mode "HideShow")
  (diminish 'yas-minor-mode "YAS")
  (diminish 'company-mode "Comp")
  (diminish 'which-key-mode "WhKey")
  (diminish 'helm-migemo-mode "HelmMigemo")
  (diminish 'undo-tree-mode "UndoTree")
  (diminish 'super-save-mode "SSave"))

(use-package eldoc-eval
  :defer t
  :custom
  (eldoc-in-minibuffer-mode-lighter " EldocEval"))

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  (evil-define-key 'normal dashboard-mode-map (kbd "j") 'dashboard-next-line)
  (evil-define-key 'normal dashboard-mode-map (kbd "k") 'dashboard-previous-line)
  )

;;; Linux specific setup
(when (eq system-type 'gnu/linux)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))

  (set-face-attribute 'default nil
                      :family "Ricty Discord"
                      :height 120)
  (set-face-attribute 'variable-pitch nil
                      :family "Migu 1VS"
                      :height 105)
  (set-fontset-font t 'cyrillic (font-spec :family "DejaVu Sans"))
  (set-fontset-font t 'greek (font-spec :family "DejaVu Sans"))
  (add-hook 'text-mode-hook
            '(lambda ()
               (buffer-face-set 'variable-pitch)))
  (add-hook 'Info-mode-hook
            '(lambda ()
               (buffer-face-set 'variable-pitch)))
  (defun set-face-font-height (size)
    (interactive "nSize: ")
    (set-face-attribute 'default nil
                        :height size))
  (use-package ispell
    :defer t
    :custom
    (ispell-program-name "aspell")
    (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
    :config
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

  (use-package flyspell
    :defer t
    :config
    (unbind-key "C-M-i" flyspell-mode-map)
    (unbind-key "C-;" flyspell-mode-map)
    (unbind-key "C-," flyspell-mode-map))

  (use-package autodisass-java-bytecode
    :ensure t
    :defer t)
  (use-package meghanada
    :ensure t
    :defer t
    :hook
    (java-mode . meghanada-mode))

  (use-package multi-term
    :ensure t
    :bind (("C-l !" . multi-term-next))
    :custom
    (multi-term-program "/usr/bin/zsh")
    :init
    (push '(term-mode :position bottom :width 60)
        popwin:special-display-config)))
