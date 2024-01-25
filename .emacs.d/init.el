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
      '(("melpa" . 30)
        ("melpa-stable" . 20)
        ("gnu" . 10)))
(package-initialize)

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
(save-place-mode +1)
(display-time)
(line-number-mode 1)
(column-number-mode 1)
(setq gc-cons-threshold (* 100 gc-cons-threshold))
(setq gc-cons-percentage 0.2)
(setq read-process-output-max (* 1024 1024))
(setq message-log-max 100000)
(setq enable-recursive-minibuffers t)
(setq use-dialog-box nil)
(defalias 'message-box 'message)
(setq history-length 10000)
(setq echo-keystrokes 0.1)
(setq large-file-warning-threshold (* 25 1024 1024))
(setq use-short-answers t)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvar default-tab-width 4)
(setq tab-stop-list '(4 8 12))
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
(if window-system
    (progn
      (add-to-list 'default-frame-alist '(alpha-background . 90))
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (menu-bar-mode 0)))
(recentf-mode 1)
(tab-bar-mode -1)
(pixel-scroll-precision-mode +1)
(setopt pixel-scroll-precision-large-scroll-height 40.0)

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
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(defun copy-buffer-file-path ()
  (interactive)
  (kill-new (buffer-file-name)))

;;; Fix copy/paste in Wayland
;; credit: yorickvP on Github
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(setopt debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :init
  (when (and (not pgtk-initialized) (eq system-type 'gnu/linux) (window-system))
    (defun raise-frame-with-wmctrl (&optional frame)
      (call-process "wmctrl" nil nil nil "-i" "-R"
                    (frame-parameter (or frame (selected-frame)) 'outer-window-id)))
    (advice-add 'raise-frame :after #'raise-frame-with-wmctrl)
    (defun iconify-emacs-when-server-is-done ()
      (unless server-clients (iconify-frame)))
    (add-hook 'server-switch-hook #'raise-frame)
    (add-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  :config
  (unless (server-running-p)
    (server-start)))

(use-package auto-compile
  :ensure t
  :pin melpa-stable
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
  (enable-theme 'doom-dracula)
  (doom-themes-set-faces 'doom-dracula
    '(font-lock-variable-name-face :foreground (doom-color 'cyan)))
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
        (propertize (format " [%s]" (projectile-default-project-name (projectile-project-root)))
                    'face (if (doom-modeline--active)
                              '(:foreground "#8cd0d3" :weight bold)
                            'mode-line-inactive))
      ""))

  (doom-modeline-def-segment linum-colnum
    "Display current linum/colnum"
    (propertize (format " Ln %s, Col %s"
                        (format-mode-line "%l")
                        (format-mode-line "%c"))
                'face (if (doom-modeline--active)
                          '(:foreground "#8cd0d3" :weight bold)
                        'mode-line-inactive)))

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
    (if (eq major-mode 'python-mode)
        (let ((venv-name (if (or (not (boundp 'pyvenv-virtual-env-name))
                                 (eq pyvenv-virtual-env-name nil))
                             "GLOBAL"
                           pyvenv-virtual-env-name)))
          (propertize (format " [%s]" venv-name)
                      'face (if (doom-modeline--active)
                                '(:foreground "#f0dfaf" :weight bold)
                              'mode-line-inactive)))
      ""))

  (with-eval-after-load 'evil
    (doom-modeline-def-segment evil-state-seg
      "Display current Evil State."
      (propertize (format " <%s>" (upcase (substring (symbol-name evil-state) 0 1)))
                  'face '(:weight bold)))
    (doom-modeline-def-modeline 'simple
      ;; '(bar evil-state-seg matches remote-host buffer-info-simple linum-colnum pdf-pages)
      '(bar evil-state-seg matches remote-host buffer-info-simple linum-colnum)
      '(projectile-project-name vcs checker battery datetime)))

  (doom-modeline-def-modeline 'verbose
    '(bar matches remote-host buffer-info-simple buffer-size)
    '(major-mode minor-modes python-venv buffer-encoding))

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
  (setq doom-modeline-checker-simple-format nil))

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

  (defun evil-forward-par (origfun arg)
    (funcall origfun)
    (if (not (or (evil-emacs-state-p) (evil-end-of-line-p)))
        (backward-char)))

  (defun evil-backward-par (origfun arg)
    (if (evil-end-of-line-p)
        (forward-line)
      (forward-char))
    (funcall origfun))

  (advice-add 'paredit-forward :around #'evil-forward-par)
  (advice-add 'paredit-backward :around #'evil-backward-par)
  ;; (advice-add 'forward-sexp :around #'evil-forward-par)
  ;; (advice-add 'backward-sexp :around #'evil-backward-par)
  )

(use-package posframe
  :ensure t
  :defer t)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-m" . 'mc/edit-lines)))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode +1)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/"))))

(use-package evil
  :ensure t
  :custom
  (evil-echo-state nil)
  (evil-undo-system 'undo-tree)
  :init
  (defun evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  :config
  (evil-mode 1)
  (defun my/evil-scroll-line-down-1 ()
    (interactive)
    (evil-scroll-line-down 1)
    (forward-line 1))
  (defun my/evil-scroll-line-up-1 ()
    (interactive)
    (evil-scroll-line-up 1)
    (forward-line -1))
  (bind-keys :map evil-normal-state-map
             ("M-." . xref-find-definitions)
             ("J" . my/evil-scroll-line-down-1)
             ("K" . my/evil-scroll-line-up-1)
             ("C-e" . end-of-line)
             ("C-t" . other-window-or-split)
             :map evil-insert-state-map
             ("C-t" . other-window-or-split)
             ("C-e" . end-of-line))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")
  )

(use-package evil-mode-line
  :init
  (use-package url)
  (let* ((site-lisp-dir "~/.emacs.d/elisp")
         (mode-line-color-file (concat site-lisp-dir "/mode-line-color.el"))
         (evil-mode-line-file (concat site-lisp-dir "/evil-mode-line.el")))
    (unless (file-directory-p site-lisp-dir)
      (mkdir site-lisp-dir))
    (add-to-list 'load-path site-lisp-dir)
    (unless (file-exists-p mode-line-color-file)
      (url-copy-file "https://raw.githubusercontent.com/tarao/elisp/master/mode-line-color.el" mode-line-color-file))
    (unless (file-exists-p evil-mode-line-file)
      (url-copy-file "https://raw.githubusercontent.com/tarao/evil-plugins/master/evil-mode-line.el" evil-mode-line-file)))
  :custom (evil-mode-line-color `((normal . ,(doom-color 'bg-alt))
                                  (insert . ,(doom-darken (doom-color 'green) 0.5))
                                  (visual . ,(doom-color 'dark-blue))
                                  (emacs . ,(doom-color 'magenta)))))

(use-package which-key
  :ensure t
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 2.0)
  (which-key-idle-secondary-delay 1.0)
  :config
  (which-key-setup-side-window-right))

(use-package ripgrep
  :ensure t
  :defer t
  :custom
  (ripgrep-executable "/usr/bin/rg")
  (ripgrep-arguments '("-S")))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-S-i" . embark-act)
   ("M-i" . embark-dwim)))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("M-i" . consult-line-thing-at-point)
   ("C-M-g" . consult-ripgrep))
  :config
  ;; cosult-line-thing-at-point
  (consult-customize consult-line :add-history (seq-some #'thing-at-point '(region symbol)))
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize consult-line-thing-at-point :initial (thing-at-point 'symbol)))

(use-package consult-ghq
  :ensure t
  :defer t)

(use-package embark-consult
  :ensure t
  )

(use-package wgrep
  :ensure t
  )

(use-package ace-jump-mode
  :ensure t
  :defer t)

(use-package ace-isearch
  :ensure t
  :defer t
  :custom
  (ace-isearch-function 'ace-jump-char-mode)
  (ace-isearch-use-function-from-isearch nil)
  (ace-isearch-jump-delay 0.8)
  :init
  (global-ace-isearch-mode +1))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  :config
  (evil-define-key 'insert company-mode-map (kbd "C-n") 'company-select-next)
  (evil-define-key 'insert company-mode-map (kbd "C-p") 'company-select-previous)
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
  :ensure t
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
  (python-ts-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (smartrep-define-key
      flycheck-mode-map "C-c !"
    '(("n" . flycheck-next-error)
      ("p" . flycheck-previous-error)))
  (push '(flycheck-error-list-mode :position bottom :width 5 :noselect t)
        popwin:special-display-config))

(use-package smerge-mode
  :defer t
  :config
  (smartrep-define-key
      smerge-mode-map "C-c ^"
    '(("n" . smerge-next)
      ("p" . smerge-prev)
      ("l" . smerge-keep-lower)
      ("u" . smerge-keep-upper)
      ("b" . smerge-keep-base)
      ("a" . smerge-keep-all)
      ("E" . smerge-ediff))))

(use-package highlight-indent-guides
  :ensure t
  :hook
  (python-mode . highlight-indent-guides-mode)
  (python-ts-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package pangu-spacing
  :ensure t
  :custom
  (pangu-spacing-real-insert-separtor t)
  :config
  ;; chinse-two-byte→japanese に置き換えて日本語で使う
  (setq pangu-spacing-chinese-before-english-regexp
        (rx (group-n 1 (category japanese))
            (group-n 2 (in "a-zA-Z0-9"))))
  (setq pangu-spacing-chinese-after-english-regexp
        (rx (group-n 1 (in "a-zA-Z0-9"))
            (group-n 2 (category japanese)))))

(use-package lsp-mode
  :ensure t
  :hook
  (python-mode . lsp)
  (python-ts-mode . lsp)
  (perl-mode . lsp)
  (cperl-mode . lsp)
  :custom
  ;; general
  (lsp-idle-delay 2)
  (lsp-ui-sideline-enable nil)
  ;; Python
  ;; use flake8 instead of pycodestyle/pyflakes/mccabe
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-flake8-enabled t)
  ;; use pylint
  (lsp-pylsp-plugins-pylint-enabled t)
  ;; use yapf
  (lsp-pylsp-plugins-yapf-enabled t)
  :config
  (use-package lsp-ui
    :ensure t
    :preface
    (defun my/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
          ("C-c d" . my/toggle-lsp-ui-doc))
    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'at-point)
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(use-package python-mode
  :defer t
  :config
  (define-key python-mode-map [remap left-word] #'python-indent-shift-left)
  (define-key python-mode-map [remap right-word] #'python-indent-shift-right))

(use-package lsp-java
  :defer t
  :pin melpa-stable
  :ensure t
  :hook
  (java-mode . lsp)
  :custom
  (lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"))
(use-package dap-mode
  :defer t
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))
(use-package dap-java
  :defer t
  :after lsp-java)

;;; dired
(use-package lv :ensure t :defer t)
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
(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package direx
  :ensure t
  :init
  (defun my/direx-open ()
    (interactive)
    (or (ignore-errors
          (direx-project:jump-to-project-root-other-window))
        (direx:jump-to-directory-other-window)))
  (defun my/direx-dwim ()
    (interactive)
    (if (derived-mode-p 'direx:direx-mode)
        (kill-buffer)
      (my/direx-open)))
  :bind
  (("<f8>" . my/direx-dwim))
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

;; (use-package recentf-ext :ensure t)

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
  (imenu-list-focus-after-activation t)
  :hook
  (imenu-list-after-jump . imenu-list-smart-toggle)
  :bind
  ("C-;" . imenu-list-smart-toggle)
  :config
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "j") 'next-line)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "k") 'previous-line)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "RET") 'imenu-list-goto-entry))

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
  :bind (("C-l m s" . magit-status)
         ("C-l m l c" . magit-log-current)
         ("C-l m l b" . magit-log-buffer-file))
  :init
  (defun surpress-iconify (&rest arg)
    (remove-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  (defun apply-iconify (&rest arg)
    (add-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  (advice-add 'magit-run-git-with-editor :before #'surpress-iconify)
  (advice-add 'with-editor-finish :after #'apply-iconify))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (python-ts-mode . rainbow-delimiters-mode)
  )

(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
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
  (python-mode . electric-operator-mode)
  (python-ts-mode . electric-operator-mode))

(use-package expand-region
  :ensure t
  :bind
  (("C-M-]" . er/expand-region)
   ("C-M-:" . er/contract-region)))

(use-package origami
  :ensure t
  :bind
  ("C-l o" . origami-recursively-toggle-node)
  )

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
  ("C-l C-s" . highlight-symbol)
  ("M-n" . highlight-symbol-next)
  ("M-p" . highlight-symbol-prev))

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
  :bind-keymap ("C-l p" . projectile-command-map)
  :config
  (when (executable-find "ghq")
    (projectile-load-known-projects)
    (setq projectile-known-projects
          (delete-dups
           (append projectile-known-projects
                   (mapcar
                    (lambda (x) (abbreviate-file-name x))
                    (split-string (shell-command-to-string "ghq list --full-path"))))))))

(use-package yasnippet
  :ensure t
  :commands (yas-expand)
  :hook
  (prog-mode . yas-minor-mode)
  (python-ts-mode . yas-minor-mode)
  :bind (("C-<tab>" . yas-expand))
  :config
  (use-package yasnippet-snippets :ensure t))

;;; C
(defun c/c++-mode-setup ()
  "Hook function to setup `c-mode' and `c++-mode'."
  (projectile-mode)
  (hs-minor-mode 1))
(add-hook 'c-mode-hook #'c/c++-mode-setup)
(add-hook 'c++-mode-hook #'c/c++-mode-setup)

(use-package rtags
  :ensure t
  :defer t
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
  :after (rtags)
  :bind (
         :map c++-mode-map
         ("C-l i" . clang-format-buffer)
         :map c-mode-map
         ("C-l i" . clang-format-buffer)
         )
  )

(use-package hideif
  :defer t
  :init
  (add-hook 'c++-mode-hook 'hide-ifdef-mode)
  (add-hook 'c-mode-hook 'hide-ifdef-mode))

;; (use-package js2-mode
;;   :ensure t
;;   :defer t
;;   :mode (("\\.js\\'" . js2-mode))
;;   )
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.js\\'" . web-mode)))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package cperl-mode
  :mode (("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
  :init
  (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode)))

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
  :custom
  (plantuml-jar-path "~/opt/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  :config
  (push '("*PLANTUML Preview*" :position right :width 50 :noselect t)
        popwin:special-display-config))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "multimarkdown")
  )
(setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

(use-package markdown-preview-mode
  :ensure t
  :defer t)

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
  ((rust-mode . racer-mode)
   (racer-mode . eldoc-mode)
   (racer-mode . company-mode))
  :config
  (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition))

(use-package flycheck-rust
  :ensure t
  :hook
  ((rust-mode . flycheck-mode)
   (rust-mode . flycheck-rust-setup)))

(use-package tex-jp
  :ensure auctex
  :after tex-mode
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-default-mode 'japanese-latex-mode)
  (japanese-TeX-engine-default 'uptex)
  (japanese-LaTeX-default-style "jsarticle")
  (TeX-engine 'uptex)
  (TeX-PDF-from-DVI "Dvipdfmx")
  (TeX-view-program-list '(("Evince" "/usr/bin/evince %o")))
  (TeX-view-program-selection '((output-pdf "Evince")))
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
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
  (TeX-source-correlate-mode +1)
  (use-package pdf-sync)
  (bind-key "C-c s s" 'pdf-sync-forward-search LaTeX-mode-map)
  (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
    (delq (assoc command TeX-command-list) TeX-command-list))
  (LaTeX-math-mode +1)
  (turn-on-reftex)
  )

(use-package org
  :defer t
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
  (setq org-latex-default-class "bxjsarticle")
  ;; org-latex-classes
  (add-to-list 'org-latex-classes
               '("bxjsarticle"
                 "\\documentclass[autodetect-engine,dvipdfmx-if-dvi,ja=standard,a4paper,12pt]{bxjsarticle}"
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
  :defer t
  :init
  (add-hook 'yaml-mode-hook #'(lambda () (buffer-face-set 'default)))
  (defun my/yaml-indent-shift-right (beg end)
    (interactive "r")
    (let ((tab-stop-list '(2 4 6))
          (deactivate-mark nil))
      (indent-rigidly-right-to-tab-stop beg end)))
  (defun my/yaml-indent-shift-left (beg end)
    (interactive "r")
    (let ((tab-stop-list '(2 4 6))
          (deactivate-mark nil))
      (indent-rigidly-left-to-tab-stop beg end)))
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  :bind
  (:map yaml-mode-map
        ("M-<right>" . my/yaml-indent-shift-right)
        ("M-<left>" . my/yaml-indent-shift-left))
  :config
  (buffer-face-set 'default))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package open-junk-file
  :ensure t
  :custom
  (open-junk-file-format "~/.junk/%Y/%m/%d-%H%M%S.")
  :bind
  (("C-l j" . open-junk-file))
  )

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
  (defun dashboard-jump-to-recent-files ()
    (interactive)
    (let ((search-label "Recent Files:"))
      (unless (search-forward search-label (point-max) t)
        (search-backward search-label (point-min) t))
      (back-to-indentation)))
  :config
  (evil-define-key 'normal dashboard-mode-map (kbd "j") 'dashboard-next-line)
  (evil-define-key 'normal dashboard-mode-map (kbd "k") 'dashboard-previous-line)
  (evil-define-key 'normal dashboard-mode-map (kbd "r") 'dashboard-jump-to-recent-files)
  )

;;; Linux specific setup
(when (eq system-type 'gnu/linux)
  (use-package exec-path-from-shell
    :ensure t
    :custom
    (exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS"))
    :config
    (exec-path-from-shell-initialize))

  (use-package pdf-tools
    :ensure t
    :defer t
    :init
    (pdf-loader-install))

  (set-face-attribute 'default nil
                      :family "Ricty Discord"
                      :height 120)
  (set-face-attribute 'variable-pitch nil
                      :family "Migu 1VS"
                      :height 105)
  (if window-system
      (progn
        (set-fontset-font t 'cyrillic (font-spec :family "DejaVu Sans"))
        (set-fontset-font t 'greek (font-spec :family "DejaVu Sans"))))

  (add-hook 'text-mode-hook
            #'(lambda ()
                (buffer-face-set 'variable-pitch)))
  (add-hook 'Info-mode-hook
            #'(lambda ()
                (buffer-face-set 'variable-pitch)))
  (defun set-face-font-height (size)
    (interactive "nSize: ")
    (set-face-attribute 'default nil
                        :height size)
    (set-face-attribute 'variable-pitch nil
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
  )
