;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-prefer-newer t)
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (setopt package-install-upgrade-built-in t)

(require 'package)
(setopt package-native-compile t)
;; (setq package-archives
;;       '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities
      '(("gnu" . 30)
        ("nongnu" . 20)
        ("melpa" . 10)))
;; (package-initialize)

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-l"))
(bind-keys ("C-l C-l" . recenter-top-bottom)
           ("C-l C-x" . server-edit)
           ("C-l C-<tab>" . tab-to-tab-stop)
           ("C-M-y" . duplicate-dwim))

;;; customize
(set-language-environment "Japanese")
(auto-compression-mode +1)
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

(savehist-mode 1)
(save-place-mode 1)
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
(setq large-file-warning-threshold (* 500 1024 1024))
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
(setq require-final-newline t)
;; デフォルト色付け
(use-package generic-x)
(show-paren-mode +1)
(pixel-scroll-precision-mode +1)
(setopt pixel-scroll-precision-large-scroll-height 40)
(global-auto-revert-mode +1)

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

(defun copy-buffer-file-path (use-file-name-only)
  "Copy the current buffer's file path to the kill ring.
If called with a prefix argument (C-u), copy only the file name (without path)."
  (interactive "P")
  (if-let ((file-path (buffer-file-name)))
      (let ((text-to-copy (if use-file-name-only
                              (file-name-nondirectory file-path)
                            file-path)))
        (kill-new text-to-copy)
        (message "Copied: %s" text-to-copy))
    (message "This buffer is not associated with a file")))

(defun copy-project-buffer-file-path ()
  (interactive)
  (let* ((project-root (file-local-name (abbreviate-file-name
                                         (or (when-let ((project (project-current)))
                                               (expand-file-name
                                                (if (fboundp 'project-root)
                                                    (project-root project)
                                                  (car (with-no-warnings (project-roots project))))))
                                             default-directory))))
         (project-buffer-file-path
          (concat
           ;; Project directory
           (concat (file-name-nondirectory (directory-file-name project-root)) "/")
           ;; relative path
           (when-let (relative-path (file-relative-name
                                     (or (file-name-directory buffer-file-name)
                                         "./")
                                     project-root))
             (if (string= relative-path "./")
                 ""
               relative-path))
           ;; File name
           (file-name-nondirectory buffer-file-name))))
    (kill-new project-buffer-file-path)
    (message "copied: %s" project-buffer-file-path)))

;; Resize the whole frame, and not only a window
;; Adapted from https://stackoverflow.com/a/24714383/5103881
(defun kle/zoom-frame (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (face-attribute 'default :font frame))
         (size (font-get font :size))
         (amt (or amt 1))
         (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    (message "Frame's font new size: %d" new-size)))

(defun kle/zoom-frame-out (&optional amt frame)
  "Call `kle/zoom-frame' with negative argument."
  (interactive "p")
  (kle/zoom-frame (- (or amt 1)) frame))

(global-set-key (kbd "C-x C-+") 'kle/zoom-frame)
(global-set-key (kbd "C-x C--") 'kle/zoom-frame-out)

(defun kle/ensure-package-vc-install (url)
  "package-vc-install if not yet installed."
  (let ((elpa-lisp-dir "~/.emacs.d/elpa"))
    (unless (file-directory-p (concat elpa-lisp-dir "/" (car (last (split-string url "/"))) "/"))
      (package-vc-install url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :init
  (when (and (not (boundp 'pgtk-initialized)) (eq system-type 'gnu/linux) (window-system))
    (defun raise-frame-with-wmctrl (&optional frame)
      (call-process "wmctrl" nil nil nil "-i" "-R"
                    (frame-parameter (or frame (selected-frame)) 'outer-window-id)))
    (advice-add 'raise-frame :after #'raise-frame-with-wmctrl))
  (defun iconify-emacs-when-server-is-done ()
    (unless server-clients (iconify-frame)))
  (add-hook 'server-switch-hook #'raise-frame)
  (add-hook 'server-done-hook #'iconify-emacs-when-server-is-done)
  :hook (emacs-startup . server-start))

(use-package auto-compile
  :ensure t
  ;; :pin melpa-stable
  :hook
  (emacs-startup . auto-compile-on-load-mode)
  (emacs-startup . auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package super-save
  :ensure t
  :hook (emacs-startup . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t))

(use-package recentf
  :custom (recentf-auto-cleanup 10)
  :config
  ;; recentf の メッセージをエコーエリアに表示しない
  (defun recentf-save-list-inhibit-message (orig-func &rest args)
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  (advice-add 'recentf-cleanup   :around 'recentf-save-list-inhibit-message)
  (advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message)
  )

(use-package tab-bar
  :after evil
  :custom
  (tab-bar-show 1)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  :config
  (evil-define-key 'normal global-map (kbd "T") 'tab-new)
  (evil-define-key 'normal global-map (kbd "C-S-t") 'tab-close)
  (evil-define-key 'normal global-map (kbd "L") 'tab-next)
  (evil-define-key 'normal global-map (kbd "H") 'tab-previous)
  )

(use-package doom-themes
  :ensure t
  :hook (emacs-startup . window-divider-mode)
  :custom (window-divider-default-right-width 10)
  :config
  (load-theme 'doom-dracula t)
  (enable-theme 'doom-dracula)
  (doom-themes-set-faces 'doom-dracula
    '(font-lock-variable-name-face :foreground (doom-color 'cyan))))

(use-package doom-modeline
  :ensure t
  :hook
  (emacs-startup . doom-modeline-mode)
  :commands (doom-modeline-def-modeline doom-modeline-def-segment)
  :init
  (defun remove-padding-zero (num)
    (if (string= (substring num 0 1) "0")
        (substring num 1)
      num))

  (doom-modeline-def-segment my-buffer-size
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

  ;; (doom-modeline-def-segment linum-colnum
  ;;   "Display current linum/colnum"
  ;;   (propertize (format " Ln %s, Col %s"
  ;;                       (format-mode-line "%l")
  ;;                       (format-mode-line "%c"))
  ;;               'face (if (doom-modeline--active)
  ;;                         '(:foreground "#8cd0d3" :weight bold)
  ;;                       'mode-line-inactive)))

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

  (doom-modeline-def-segment csv-index
    "Display current csv column index"
    (if (derived-mode-p 'csv-mode)
        (format " F%d" (csv--field-index))
      ""))

  ;; ;; you can use featurep to check if library is loaded or not
  ;; (with-eval-after-load 'evil
  ;;   (doom-modeline-def-segment evil-state-seg
  ;;     "Display current Evil State."
  ;;     (propertize (format " <%s>" (upcase (substring (symbol-name evil-state) 0 1)))
  ;;                 'face '(:weight bold)))
  ;;   )
  (doom-modeline-def-modeline 'simple
    ;; '(bar evil-state-seg matches remote-host buffer-info-simple linum-colnum pdf-pages)
    ;; '(bar evil-state-seg matches remote-host buffer-info-simple linum-colnum)
    ;; '(bar modals matches remote-host buffer-info-simple buffer-position)
    '(bar modals matches remote-host buffer-info buffer-position csv-index)
    '(projectile-project-name vcs check battery datetime)
    )

  (doom-modeline-def-modeline 'verbose
    '(bar matches remote-host buffer-info-simple my-buffer-size)
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
  (defun kle/paredit-forward-advice (orig-fun &rest args)
    (if (and (bound-and-true-p evil-mode)
             (or (evil-normal-state-p) (evil-visual-state-p)))
        (progn
          (funcall orig-fun)
          (unless (or (eolp) (bobp))  ;; 行末・行頭でなければ1文字戻る
            (backward-char)))
      (apply orig-fun args)))

  (defun kle/paredit-backward-advice (orig-fun &rest args)
    (if (and (bound-and-true-p evil-mode)
             (or (evil-normal-state-p) (evil-visual-state-p)))
        (progn
          (if (eolp)
              (forward-line 1)  ;; 行末なら次の行の行頭へ
            (forward-char))     ;; それ以外なら1文字先へ
          (funcall orig-fun))
      (apply orig-fun args)))
  (advice-add 'paredit-forward :around #'kle/paredit-forward-advice)
  (advice-add 'paredit-backward :around #'kle/paredit-backward-advice))

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
  (defun kle/evil-scroll-line-down-1 ()
    (interactive)
    (evil-scroll-line-down 1)
    (forward-line 1))
  (defun kle/evil-scroll-line-up-1 ()
    (interactive)
    (evil-scroll-line-up 1)
    (forward-line -1))
  (bind-keys :map evil-normal-state-map
             ("M-." . xref-find-definitions)
             ("J" . kle/evil-scroll-line-down-1)
             ("K" . kle/evil-scroll-line-up-1)
             ("C-e" . end-of-line)
             ("C-t" . other-window-or-split)
             :map evil-insert-state-map
             ("C-t" . other-window-or-split)
             ("C-e" . end-of-line))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk"))

(use-package evil-mode-line
  :init
  (let* ((site-lisp-dir "~/.emacs.d/elisp")
         (mode-line-color-file (concat site-lisp-dir "/mode-line-color.el"))
         (evil-mode-line-file (concat site-lisp-dir "/evil-mode-line.el")))
    (unless (and (file-exists-p mode-line-color-file) (file-exists-p evil-mode-line-file))
      (use-package url)
      (unless (file-directory-p site-lisp-dir)
        (mkdir site-lisp-dir))
      (url-copy-file "https://raw.githubusercontent.com/tarao/evil-plugins/master/evil-mode-line.el" evil-mode-line-file)
      (url-copy-file "https://raw.githubusercontent.com/tarao/elisp/master/mode-line-color.el" mode-line-color-file))
    (add-to-list 'load-path site-lisp-dir))
  :custom (evil-mode-line-color `((normal . ,(doom-color 'bg-alt))
                                  (insert . ,(doom-darken (doom-color 'green) 0.5))
                                  (visual . ,(doom-color 'dark-blue))
                                  (emacs . ,(doom-color 'magenta)))))

(use-package which-key
  :ensure t
  :pin melpa
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 2.0)
  (which-key-idle-secondary-delay 1.0)
  :config
  (which-key-setup-side-window-right))

(use-package migemo
  :ensure t
  :custom
  (migemo-isearch-enable-p nil)
  (migemo-directory (seq-find #'file-exists-p
                              '("/usr/share/cmigemo/utf-8/migemo-dict"
                                (expand-file-name "~/opt/migemo/dict/utf-8/migemo-dict"))))
  :config
  ;; C-u で migemo を有効にする isearch
  (defun kle/isearch-forward-migemo (arg)
    "通常は通常のisearch。C-uでmigemoが有効になる。"
    (interactive "P")
    (let ((migemo-isearch-enable-p arg))
      (isearch-forward)))
  (defun kle/isearch-backward-migemo (arg)
    "通常は通常のisearch。C-uでmigemoが有効になる(後方検索)。"
    (interactive "P")
    (let ((migemo-isearch-enable-p arg))
      (isearch-backward)))
  (bind-key "C-s" 'kle/isearch-forward-migemo)
  (bind-key "C-r" 'kle/isearch-backward-migemo))

(use-package ripgrep
  :ensure t
  :defer t
  :custom
  (ripgrep-executable (seq-find #'file-exists-p
                                '("/usr/bin/rg"
                                  (expand-file-name "~/bin/rg"))))
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
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

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
   ("C-M-g" . consult-ripgrep)
   ("M-g g" . consult-goto-line)
   )
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

(use-package corfu
  :ensure t
  :custom ((corfu-auto t)
           (corfu-auto-delay 0)
           (corfu-auto-prefix 2)
           (corfu-cycle t)
           (corfu-on-exact-match nil)
           (tab-always-indent 'complete))
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :init (global-corfu-mode +1)
  :config
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (evil-define-key 'insert corfu-map (kbd "C-n") 'corfu-next)
  (evil-define-key 'insert corfu-map (kbd "C-p") 'corfu-previous)
  )

(use-package cape
  :ensure t
  :bind (("C-:" . cape-dabbrev))
  )

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
    '(("<tab>" . tab-to-tab-stop)))
  (smartrep-define-key
      global-map "C-x"
    '(("C-+" . kle/zoom-frame)
      ("C--" . kle/zoom-frame-out)))
  )

(use-package popwin
  :ensure t
  :config
  (popwin-mode +1))

(use-package xref
  :defer t
  :init
  (push '("*xref*" :position bottom :width 5)
        popwin:special-display-config))

(use-package flymake
  :ensure t
  :pin gnu
  :commands (flymake-show-buffer-diagnostics flymake-goto-next-error flymake-goto-prev-error)
  :bind (("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error))
  :config
  (smartrep-define-key
      flymake-mode-map "C-c !"
    '(("n" . flymake-goto-next-error)
      ("p" . flymake-goto-prev-error)))
  (push '(flymake-diagnostics-buffer-mode :position bottom :width 5 :noselect t)
        popwin:special-display-config))

(use-package flymake-ruff
  :ensure t
  :init
  (defun kle/flymake-ruff-load-python-ts-mode ()
    "check major mode before load flymake-ruff"
    (when (eq major-mode 'python-ts-mode)
      (flymake-ruff-load)))
  :hook (eglot-managed-mode . kle/flymake-ruff-load-python-ts-mode))

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

(use-package pangu-spacing
  :ensure t
  :defer t
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

(use-package python
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :hook
  (python-ts-mode-hook . electric-operator-mode)
  (python-mode-hook . electric-operator-mode)
  :config
  (smartrep-define-key
      python-mode-map "C-c"
    '(("<" . python-indent-shift-left)
      (">" . python-indent-shift-right)))
  (smartrep-define-key
      python-ts-mode-map "C-c"
    '(("<" . python-indent-shift-left)
      (">" . python-indent-shift-right)))
  (defun ruff-fix-buffer ()
    "Use ruff to fix lint violations in the current buffer."
    (interactive)
    (shell-command-to-string (format "ruff check --fix %s" (buffer-file-name)))
    (shell-command-to-string (format "ruff format %s" (buffer-file-name)))
    (revert-buffer t t t)))

(use-package eglot
  :ensure t
  :pin gnu
  :defer t
  :config
  (unless (require 'eglot-booster nil t)
    (package-vc-install "https://github.com/jdtsmith/eglot-booster"))
  (use-package eglot-booster
    :config (eglot-booster-mode +1))
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
  (defun my-reorder-eldoc-functions ()
    "Ensure `flymake-eldoc-function` is the first in `eldoc-documentation-functions`."
    (when (and (boundp 'eldoc-documentation-functions)
               (listp eldoc-documentation-functions))
      (let ((flymake-fn 'flymake-eldoc-function))
        (setq eldoc-documentation-functions
              (cons flymake-fn (remove flymake-fn eldoc-documentation-functions))))))
  (add-hook 'eglot-managed-mode-hook #'my-reorder-eldoc-functions)
  (setq-default eglot-workspace-configuration
                '(:yaml (:customTags ["!Sub scalar" "!Sub sequence" "!GetAtt scalar" "!Ref scalar"]))))

;; (use-package eglot-java
;;   :ensure t
;;   :init
;;   (defun kle/eglot-java--install-lsp-server ()
;;     "Install specific version of the Eclipse JDT LSP server."
;;     (interactive)
;;     (let* ((destination-dir              (concat user-emacs-directory "share/eclipse.jdt.ls"))
;;            (dest-dir                     (expand-file-name destination-dir))
;;            (download-metadata            (eglot-java--parse-jdtls-download-metadata
;;                                           (eglot-java--read-json-from-url eglot-java-eclipse-jdt-ls-dl-metadata-url)))
;;            (download-url                 "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
;;            (download-version             "0.57.0")
;;            (dest-filename                (file-name-nondirectory download-url))
;;            (dest-abspath                 (expand-file-name dest-filename dest-dir))
;;            (dest-versionfile             (expand-file-name eglot-java-filename-version-jdtls dest-dir))
;;            (large-file-warning-threshold nil))
;;       (message "Installing Eclipse JDT LSP server, please wait...")
;;       (eglot-java--download-file download-url dest-abspath)

;;       (message "Extracting Eclipse JDT LSP archive, please wait...")
;;       (let ((b (find-file dest-abspath)))
;;         (switch-to-buffer b)
;;         (goto-char (point-min))
;;         (tar-untar-buffer)
;;         (kill-buffer b))
;;       (delete-file dest-abspath)

;;       (eglot-java--record-version-info download-version dest-versionfile)

;;       (message "Eclipse JDT LSP server installed in folder \n\"%s\"." dest-dir)))
;;   :custom
;;   (eglot-java-eglot-server-programs-manual-updates t))

;;; dired
(use-package lv :ensure t :defer t)
(use-package dired
  :defer t
  :custom
  ;; fix keybind for SKK
  (dired-bind-jump nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (bind-keys :map dired-mode-map
             ("C-t" . other-window-or-split)
             )
  (use-package dired-x)
  (custom-set-variables '(dired-omit-files (concat dired-omit-files "\\|^\\..+$")))
  (bind-key "C-l C-o" 'dired-omit-mode dired-mode-map)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
  (when (eq system-type 'gnu/linux)
    (custom-set-variables '(dired-listing-switches "-AFDlh --group-directories-first")))
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
  (defun kle/direx-open ()
    (interactive)
    (or (ignore-errors
          (direx-project:jump-to-project-root-other-window))
        (direx:jump-to-directory-other-window)))
  (defun kle/direx-dwim ()
    (interactive)
    (if (derived-mode-p 'direx:direx-mode)
        (kill-buffer)
      (kle/direx-open)))
  :bind
  (("<f8>" . kle/direx-dwim))
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

(use-package skk
  :ensure ddskk
  :bind
  (("C-x j" . skk-auto-fill-mode)
   ("C-x C-j" . skk-mode))
  :custom
  (skk-user-directory "~/.skk.d")
  (skk-dcomp-activate t)
  (skk-show-candidates-always-pop-to-buffer t)
  (skk-isearch-start-mode 'latin)
  (skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
  (skk-extra-jisyo-file-list
   '("~/.emacs.d/skk-get-jisyo/SKK-JISYO.jinmei"
     "~/.emacs.d/skk-get-jisyo/SKK-JISYO.fullname"
     "~/.emacs.d/skk-get-jisyo/SKK-JISYO.geo"
     "~/.emacs.d/skk-get-jisyo/SKK-JISYO.propernoun"
     "~/.emacs.d/skk-get-jisyo/SKK-JISYO.station"
     "~/.emacs.d/skk-get-jisyo/SKK-JISYO.law"
     "~/.emacs.d/skk-get-jisyo/SKK-JISYO.okinawa"))
  (skk-show-annotation t)
  (skk-annotation-delay 0.01)
  (skk-show-candidates-nth-henkan-char 3)
  :config
  (use-package skk-hint)
  (use-package skk-study)
  ;; Isearch setting.
  (defun skk-isearch-setup-maybe ()
    (require 'skk-vars)
    (when (or (eq skk-isearch-mode-enable 'always)
              (and (boundp 'skk-mode)
                   skk-mode
                   skk-isearch-mode-enable))
      (skk-isearch-mode-setup)))

  (defun skk-isearch-cleanup-maybe ()
    (require 'skk-vars)
    (when (and (featurep 'skk-isearch)
               skk-isearch-mode-enable)
      (skk-isearch-mode-cleanup)))

  (add-hook 'isearch-mode-hook #'skk-isearch-setup-maybe)
  (add-hook 'isearch-mode-end-hook #'skk-isearch-cleanup-maybe))

;; (use-package ddskk-posframe
;;   :ensure t
;;   :after skk
;;   :config
;;   (ddskk-posframe-mode 1))

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
  :pin melpa
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

(use-package magit-file-icons
  :ensure t
  :hook (magit-mode . magit-file-icons-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (python-ts-mode . rainbow-delimiters-mode))

(use-package anzu
  :ensure t
  :pin melpa
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
  :commands (electric-operator-add-rules-for-mode electric-operator-get-rules-for-mode))

(use-package expreg
  :ensure t
  :bind
  (("C-M-]" . expreg-expand)
   ("C-M-:" . expreg-contract)))

;; (use-package origami
;;   :ensure t
;;   :bind
;;   ("C-l o" . origami-recursively-toggle-node)
;;   )

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
  :pin melpa
  :commands (yas-expand)
  :hook
  (prog-mode . yas-minor-mode)
  (python-ts-mode . yas-minor-mode)
  :bind (("C-<tab>" . yas-expand))
  :config
  (use-package yasnippet-snippets
    :ensure t
    :pin melpa))

;;; C
(defun c/c++-mode-setup ()
  "Hook function to setup `c-mode' and `c++-mode'."
  (projectile-mode)
  (hs-minor-mode 1))
(add-hook 'c-mode-hook #'c/c++-mode-setup)
(add-hook 'c++-mode-hook #'c/c++-mode-setup)


(use-package hideif
  :defer t
  :init
  (add-hook 'c++-mode-hook 'hide-ifdef-mode)
  (add-hook 'c-mode-hook 'hide-ifdef-mode))

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

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "multimarkdown"))
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
   (racer-mode . eldoc-mode))
  :config
  (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition))

(use-package auctex
  :ensure t
  :mode (("\\.tex\\'" . TeX-tex-mode)
         ("\\.latex\\'" . TeX-tex-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  ;; use tectonic as tex engine
  (TeX-engine-alist '((tectonic                          ; engine symbol
                       "Tectonic"                        ; engine name
                       "tectonic -X compile -f plain %T" ; shell command for compiling plain TeX documents
                       "tectonic -X watch"               ; shell command for compiling LaTeX documents
                       nil                               ; shell command for compiling ConTeXt documents
                       )))
  (TeX-engine 'tectonic)
  (LaTeX-command-style '(("" "%(latex) %(extraopts)")))
  (TeX-check-TeX nil)
  :config
  (use-package tex
    :config
    (let ((tex-list (assoc "TeX" TeX-command-list))
          (latex-list (assoc "LaTeX" TeX-command-list)))
      (setf (cadr tex-list) "%(tex)"
            (cadr latex-list) "%l"))))
;;   :config
;;   (TeX-source-correlate-mode +1)
;;   (use-package pdf-sync)
;;   (bind-key "C-c s s" 'pdf-sync-forward-search LaTeX-mode-map)
;;   (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
;;     (delq (assoc command TeX-command-list) TeX-command-list))
;;   (LaTeX-math-mode +1)
;;   (turn-on-reftex)

(use-package json-ts-mode
  :defer t
  :custom (json-ts-mode-indent-offset 4))

(use-package org
  :bind
  ("C-l C-o l" . org-store-link)
  ("C-l C-o a" . org-agenda)
  ("C-l C-o c" . org-capture)
  :custom
  (org-latex-packages-alist
   '(("" "fontspec" t)
     ("" "xeCJK" t)))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t))))

(use-package ox
  :defer t
  :custom
  (org-export-default-language "ja"))

(use-package ox-latex
  :defer t
  :custom
  ;; tectonic
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process '("%latex -X compile -o %o %f"))
  (org-latex-classes
   '(("bxjsarticle"
      "\\documentclass[xelatex,ja=standard,a4paper,12pt]{bxjsarticle}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\setmainfont{Linux Libertine O}
\\setsansfont{Linux Biolinum O}
\\setmonofont{0xProto}
\\setCJKmainfont{IPAex明朝}
\\setCJKsansfont{IPAexゴシック}
\\setCJKmonofont{HackGen}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (org-latex-default-class "bxjsarticle"))

(use-package vimrc-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-hook 'yaml-mode-hook #'(lambda () (buffer-face-set 'default)))
  (defun kle/yaml-indent-shift-right (beg end)
    (interactive "r")
    (let ((tab-stop-list '(2 4 6))
          (deactivate-mark nil))
      (indent-rigidly-right-to-tab-stop beg end)))
  (defun kle/yaml-indent-shift-left (beg end)
    (interactive "r")
    (let ((tab-stop-list '(2 4 6))
          (deactivate-mark nil))
      (indent-rigidly-left-to-tab-stop beg end)))
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  :bind
  (:map yaml-mode-map
        ("M-<right>" . kle/yaml-indent-shift-right)
        ("M-<left>" . kle/yaml-indent-shift-left))
  :config
  (buffer-face-set 'default))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package csv-mode
  :ensure t
  :defer t
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsv\\'" . tsv-mode))
  (defun enable-csv-mode-for-small-files ()
    "Enable csv-mode for TSV/CSV files if they are not too large."
    (when (and buffer-file-name
               (or (string-match-p "\\.csv\\'" buffer-file-name)
                   (string-match-p "\\.tsv\\'" buffer-file-name))
               (or (not large-file-warning-threshold)
                   (< (buffer-size) large-file-warning-threshold)))
      (csv-mode +1)))
  (add-hook 'find-file-hook 'enable-csv-mode-for-small-files)
  (defun kle/smartrep-csv-setup ()
    (smartrep-define-key
        csv-mode-map "C-c" '(("l" . csv-forward-field)
                             ("h" . csv-backward-field))))
  :hook
  (csv-mode . csv-align-mode)
  (tsv-mode . csv-align-mode)
  (csv-mode . (lambda () (toggle-truncate-lines t)))
  (tsv-mode . (lambda () (toggle-truncate-lines t)))
  (csv-mode . kle/smartrep-csv-setup)
  :bind
  (:map csv-mode-map
        ("C-c l" . csv-forward-field)
        ("C-c h" . csv-backward-field))
  :custom
  (csv-align-style 'auto)
  (csv-align-max-width 200))

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
  (diminish 'highlight-symbol-mode "HighSym")
  (diminish 'smartparens-mode "SmPar")
  (diminish 'hs-minor-mode "HideShow")
  (diminish 'yas-minor-mode "YAS")
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
  (evil-define-key 'normal dashboard-mode-map (kbd "r") 'dashboard-jump-to-recent-files))

(use-package ligature
  :ensure t
  :hook
  (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 'prog-mode
                          '("->" "<-" "=>" "=>>" ">=>" "=>=" "=<<" "=<=" "<=<" "<=>"
                            ">>" ">>>" "<<" "<<<" "<>" "<|>" "==" "===" ".=" ":="
                            "#=" "!=" "!==" "=!=" "=:=" "::" ":::" ":<:" ":>:"
                            "||" "|>" "||>" "|||>" "<|" "<||" "<|||"
                            "**" "***" "<*" "<*>" "*>" "<+" "<+>" "+>" "<$" "<$>" "$>"
                            "$$" "??" "%%" "|]" "[|" "//" "///")))
(use-package copilot
  :init
  ;; check dependencies
  (use-package editorconfig
    :ensure t
    :defer t)
  (use-package jsonrpc
    :ensure t
    :defer t)
  (use-package s
    :ensure t
    :defer t)
  (use-package f
    :ensure t
    :defer t)
  ;; install via github
  (let* ((elpa-lisp-dir "~/.emacs.d/elpa")
         (copilot-file (concat elpa-lisp-dir "/copilot/copilot.el")))
    (unless (file-exists-p copilot-file)
      (package-vc-install "https://github.com/copilot-emacs/copilot.el.git" nil nil 'copilot)))
  :hook
  (python-ts-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :custom
  (warning-suppress-log-types '((copilot copilot-exceeds-max-char))))

(setopt debug-on-error t)

(setopt tramp-default-method "ssh")

;; (defun set-font-for-frame (frame)
;;   "Set the font size for a specific frame based on its display resolution."
;;   (let ((font-size (calculate-font-size-for-frame frame)))
;;     (with-selected-frame frame
;;       (set-face-attribute 'default frame :family "HackGen" :height font-size))))

;; (defun adjust-font-on-frame-events (frame)
;;   "Adjust font size when a frame is created or moved."
;;   (set-font-for-frame frame))

;; ;; Apply font settings to existing frames and new ones
;; (add-hook 'after-make-frame-functions #'adjust-font-on-frame-events)

;; ;; For the initial frame
;; (add-hook 'window-size-change-functions #'adjust-font-on-frame-events)
;; (set-face-attribute 'default nil :family "Monaspace Neon" :height 130)
;; (set-face-attribute 'default nil :family "HackGen" :height 140)
;; (set-face-attribute 'default nil :family "IBM Plex Mono" :height 130)
;; (set-face-attribute 'default nil :family "Ricty Discord" :height 120)
(set-face-attribute 'default nil :family "0xProto" :height 130)
;; (set-face-attribute 'default nil :family "Monaspace Radon" :height 130) ;; :D
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 105)
;; non-ASCII Unicode font
;; (set-fontset-font t '(#x80 . #x10ffff) (font-spec :family "Noto Mono" :size 10))
;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans Mono" :size 50))
;; (set-fontset-font t nil (font-spec :family "Noto Sans" :size 100))
(setq use-default-font-for-symbols nil)

;; (set-face-attribute 'default nil
;;                     :family "Ricty Discord"
;;                     :height 140)
;; (set-face-attribute 'variable-pitch nil
;;                     :family "Migu 1VS"
;;                     :height 105)
;; (if window-system
;;     (progn
;;       (set-fontset-font t 'cyrillic (font-spec :family "DejaVu Sans"))
;;       (set-fontset-font t 'greek (font-spec :family "DejaVu Sans"))))

;; (add-hook 'text-mode-hook
;;           #'(lambda ()
;;               (buffer-face-set 'variable-pitch)))
;; (add-hook 'Info-mode-hook
;;           #'(lambda ()
;;               (buffer-face-set 'variable-pitch)))
(defun set-face-font-height (size)
  (interactive "nSize: ")
  (set-face-attribute 'default nil
                      :height size)
  (set-face-attribute 'variable-pitch nil
                      :height size))

;;; Linux specific setup
(when (eq system-type 'gnu/linux)
  ;;; Fix copy/paste in Wayland
  ;; credit: yorickvP on Github
  ;; (if (bound-and-true-p pgtk-initialized)
  ;;     (progn
  ;;       (defvar wl-copy-process nil)
  ;;       (defun wl-copy (text)
  ;;         (setq wl-copy-process (make-process :name "wl-copy"
  ;;                                             :buffer nil
  ;;                                             :command '("wl-copy" "-f" "-n")
  ;;                                             :connection-type 'pipe
  ;;                                             :noquery t))
  ;;         (process-send-string wl-copy-process text)
  ;;         (process-send-eof wl-copy-process))
  ;;       (defun wl-paste ()
  ;;         (if (and wl-copy-process (process-live-p wl-copy-process))
  ;;             nil ; should return nil if we're the current paste owner
  ;;           (shell-command-to-string "wl-paste -n | tr -d \r")))
  ;;       (setq interprogram-cut-function 'wl-copy)
  ;;       (setq interprogram-paste-function 'wl-paste)
  ;;       ))

  ;; (use-package exec-path-from-shell
  ;;   :ensure t
  ;;   :custom
  ;;   (exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS"))
  ;;   :config
  ;;   (exec-path-from-shell-initialize))

  ;(use-package pdf-tools
  ;  :ensure t
  ;  :defer t
  ;  :init
  ;  (pdf-loader-install))
  ;; font
  ;; default ASCII font
  ;; (defun calculate-font-size-for-frame (frame)
  ;;   "Calculate the font size dynamically based on the frame's display resolution."
  ;;   (let* ((attrs (frame-monitor-attributes frame))
  ;;          (mm-width (alist-get 'mm-width attrs))       ; Display width in mm
  ;;          (pixel-width (alist-get 'geometry attrs))    ; Display geometry
  ;;          (dpi (if (and mm-width pixel-width)
  ;;                   (/ (float (nth 2 pixel-width))       ; Geometry width in pixels
  ;;                      (/ (float mm-width) 25.4))       ; Convert mm to inches
  ;;                 96))                                  ; Default to 96 DPI if unavailable
  ;;          (font-size (cond
  ;;                      ((> dpi 200) 180)  ; High DPI
  ;;                      ((> dpi 150) 140)  ; Medium DPI
  ;;                      (t 120))))        ; Low DPI
  ;;     font-size))
  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode +1))

  (use-package treesit-fold
    :init
    (let* ((elpa-lisp-dir "~/.emacs.d/elpa")
           (treesit-fold-file (concat elpa-lisp-dir "/treesit-fold/treesit-fold.el")))
      (unless (file-exists-p treesit-fold-file)
        (package-vc-install "https://github.com/emacs-tree-sitter/treesit-fold")))
    :bind
    ("C-l o" . treesit-fold-toggle))

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

  (defun file-open-file-manager ()
    "Open the directory of the current buffer's file or dired buffer in the appropriate file manager.
Uses explorer.exe for WSL with properly escaped paths and nautilus for non-WSL."
    (interactive)
    (let* ((path (or (if (derived-mode-p 'dired-mode)
                         (dired-current-directory)
                       (file-name-directory (or buffer-file-name default-directory)))
                     default-directory))
           (wsl-p (string-match-p "microsoft" (shell-command-to-string "uname -r"))) ; WSL環境かどうかを確認
           (distro (if wsl-p
                       (replace-regexp-in-string "\n" "" (shell-command-to-string "lsb_release -si")) ; WSLディストリビューション名を取得
                     nil))
           (command (if wsl-p
                        (concat "explorer.exe /root,'\\\\wsl$\\" distro
                                (replace-regexp-in-string "/" "\\" path t t) "\'")
                      (concat "nautilus --no-desktop -n " path))))
      (message "open file manager with command: %s" command)
      (start-process-shell-command "open-file-manager" nil command)))
  (bind-key "C-l C-e" 'file-open-file-manager)
  (with-eval-after-load 'dired
    (bind-key "e" 'file-open-file-manager dired-mode-map)))
