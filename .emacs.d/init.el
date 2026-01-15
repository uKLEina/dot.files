;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setopt custom-file (locate-user-emacs-file "custom.el"))
;; (setopt package-install-upgrade-built-in t)
(setopt initial-major-mode 'fundamental-mode)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; (global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-l"))
(bind-keys ("C-l C-l" . recenter-top-bottom)
           ("C-l C-x" . server-edit)
           ("C-l C-<tab>" . tab-to-tab-stop)
           ("C-M-y" . duplicate-dwim))

(defun my/setup-basic-config ()
  "基本設定のセットアップ"
  (set-language-environment "utf-8")
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8)
  (set-default 'buffer-file-coding-system 'utf-8)
  (setopt default-input-method "japanese")
  (setq read-process-output-max (* 3 1024 1024))
  (setq message-log-max 100000)
  (setq enable-recursive-minibuffers t)
  (setq use-dialog-box nil)
  (defalias 'message-box 'message)
  (setq history-length 10000)
  (setq echo-keystrokes 0.1)
  (setopt large-file-warning-threshold (* 500 1024 1024))
  (setq use-short-answers t)
  (setq visible-bell t)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setopt tab-stop-list '(4 8 12))
  (setq scroll-step 1)
  (setopt initial-scratch-message "")
  (setq delete-auto-save-files t)
  (setq frame-title-format
        '(buffer-file-name "%f"
                           (dired-directory dired-directory "%b")))
  (setopt require-final-newline t)
  (setopt backup-by-copying t)
  (setq-default indicate-buffer-boundaries 'left)
  (setopt backup-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/backup"))))
  (setopt auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backup") t)))
  (setq auto-save-timeout 15)
  (setq auto-save-interval 60))
(add-hook 'emacs-startup-hook #'my/setup-basic-config)

(defun my/setup-modes ()
  "各種モードの有効化"
  (auto-compression-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (pixel-scroll-precision-mode 1)
  (global-auto-revert-mode 1))
(run-with-idle-timer 0.5 nil #'my/setup-modes)

;; (setopt pixel-scroll-precision-large-scroll-height 40)

;;; delete path hierarchy by hierarchy in minibuffer by M-h
;;; tips; M-h works as "mark-paragraph" in a main buffer.
(defun my-minibuffer-delete-parent-directory ()
  "Delete one level of file path."
  (interactive)
  (let ((current-pt (point)))
    (when (re-search-backward "/[^/]+/?" nil t)
      (forward-char 1)
      (delete-region (point) current-pt))))
(bind-key "M-h" 'my-minibuffer-delete-parent-directory minibuffer-local-map)

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

;; ウィンドウのスマート分割
;; ヘルパー関数: 他のウィンドウがすべて指定されたユーティリティバッファか確認する
(defun my-all-other-windows-are-utility-p (utility-buffer-names)
  "Return t if all windows other than the selected one display one of the UTILITY-BUFFER-NAMES.
Return nil if there are no other windows, or if any other window
displays a buffer not in UTILITY-BUFFER-NAMES."
  (let ((other-windows (remove (selected-window) (window-list))))
    (if (null other-windows)
        nil
      (cl-every (lambda (w)
                  (let ((buf-name (buffer-name (window-buffer w))))
                    ;; member を cl-member に変更し、:test キーワード引数を使用可能にする
                    (cl-member buf-name utility-buffer-names :test #'string=)))
                other-windows))))

(defun other-window-or-split ()
  (interactive)
  (let ((utility-buffers '("*Ilist*" "*Flycheck errors*" " *NeoTree*")))
    (when (or (one-window-p)
              (my-all-other-windows-are-utility-p utility-buffers))
      (split-window-horizontally)))
  (other-window 1))
(bind-keys ("C-t" . other-window-or-split))

;;; ウィンドウ選択を変えた時に光らせて分かりやすくする
;; 直前のウィンドウを覚えておく変数
(defvar my-last-selected-window nil
  "The window object that was last selected.
Used to detect window focus changes.")

;; ウィンドウが変わった時に光らせる関数
(defun my-pulse-buffer-on-window-focus-change (frame)
  "Pulse the entire buffer when the selected window object changes."
  (ignore frame)
  (let ((current-win (selected-window))
        ;; フック実行開始時点での「直前のウィンドウ」をローカル変数に保存
        (previous-win my-last-selected-window))
    ;; 光らせる条件の判定
    (when (and (not (eq current-win previous-win))              ; ウィンドウが変わっていること
               (window-live-p current-win)                      ; 現在のウィンドウが有効なこと
               (not (window-minibuffer-p current-win))          ; ミニバッファではないこと
               (or (not previous-win)                           ; 最初の1回目はprevious-winがnilなのでok
                   (not (window-minibuffer-p previous-win))))   ; 直前のウィンドウがミニバッファではないこと
      (with-selected-window current-win
        (pulse-momentary-highlight-region (point-min) (point-max))))
    ;; 最後に選択されたウィンドウの情報を更新
    (setq my-last-selected-window current-win)))
(add-hook 'window-selection-change-functions #'my-pulse-buffer-on-window-focus-change)

;;; ファイル名をパス付きでコピー
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

;; (defun kle/ensure-package-vc-install (url)
;;   "package-vc-install if not yet installed."
;;   (let ((elpa-lisp-dir "~/.emacs.d/elpa"))
;;     (unless (file-directory-p (concat elpa-lisp-dir "/" (car (last (split-string url "/"))) "/"))
;;       (package-vc-install url))))

(setopt debug-on-error t)

;;; font
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
;; (set-face-attribute 'default nil :family "0xProto" :height 110)
(set-face-attribute 'default nil :family "ProtoGen" :height 140)
;; (set-face-attribute 'default nil :family "Monaspace Radon" :height 130) ;; :D
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 105)
;; non-ASCII Unicode font
;; (set-fontset-font t '(#x80 . #x10ffff) (font-spec :family "Noto Mono" :size 10))
;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans Mono" :size 50))
;; (set-fontset-font t nil (font-spec :family "Noto Sans" :size 100))
(setq use-default-font-for-symbols nil)

(defvar my/font-family "ProtoGen")
(defvar my/font-height 140
  "要求するデフォルトのフォント高さ（:height の単位）。")
(defvar my/font-step 10
  "フォント高さを増減する単位（:height の単位）。")
(defvar my/font-min-height 10
  "許容する最小フォント高さ。必要なければ調整または nil に。")
(defvar my/font-max-height 1000
  "許容する最大フォント高さ。必要なければ調整または nil に。")

(defun my/change-font-height (delta &optional n)
  (let* ((n (or n 1))
         (new (+ my/font-height (* delta n)))
         (new (if my/font-min-height (max my/font-min-height new) new))
         (new (if my/font-max-height (min my/font-max-height new) new)))
    (setq my/font-height new)                  ;; 希望値を更新
    (set-face-attribute 'default nil
                        :family my/font-family
                        :height new)
    (message "Requested font height: %d"new)))

(defun my/increase-font-height (n)
  (interactive "p")
  (my/change-font-height my/font-step n))

(defun my/decrease-font-height (n)
  (interactive "p")
  (my/change-font-height (- my/font-step) n))

;; キー割り当て
(global-set-key (kbd "<f5>") 'my/decrease-font-height)
(global-set-key (kbd "<f6>") 'my/increase-font-height)

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
;; 複数行をまとめる関数
;; 標準のdelete-indentationsは空白を入れるしかないので自作版
(defun kle/join-lines (beg end &optional with-space)
  "Join lines in region from BEG to END into one line.
If WITH-SPACE is non-nil (C-u), insert a single space at each join.
Otherwise, join lines with no space."
  (interactive
   (let ((with-space current-prefix-arg))
     (if (use-region-p)
         (list (region-beginning) (region-end) with-space)
       (list (line-beginning-position)
             (line-end-position 2)
             with-space))))
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert
     (if with-space
         (replace-regexp-in-string "\n+" " " (string-trim text))
       (replace-regexp-in-string "\n+" "" (string-trim text))))))
(bind-key "M-^" #'kle/join-lines)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setopt use-package-always-defer t)
(use-package package
  :custom
  (package-archive-priorities '(("gnu" . 30)
                                ("nongnu" . 20)
                                ("melpa" . 10)
                                ("melpa-stable" . 0)))
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")))

;;; configure built-in packages before package-initialize
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

(use-package recentf
  :hook (emacs-startup . recentf-mode)
  :custom
  (recentf-auto-cleanup 10)
  :config
  ;; recentf の メッセージをエコーエリアに表示しない
  (defun kle/recentf-save-list-inhibit-message (orig-func &rest args)
    (let ((inhibit-message t))
      (apply orig-func args)))
  (advice-add 'recentf-cleanup :around 'kle/recentf-save-list-inhibit-message)
  (advice-add 'recentf-save-list :around 'kle/recentf-save-list-inhibit-message)
  ;; ディレクトリも履歴に含めるようにしたいので、
  ;; Diredでディレクトリを開いたときにrecentfリストに追加する
  (defun kle/recentf-add-dired-directory ()
    (when (and (boundp 'dired-directory) dired-directory)
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook #'kle/recentf-add-dired-directory)
  ;; バッファ切り替えだけで最近開いた判定にする
  (add-hook 'buffer-list-update-hook #'recentf-track-opened-file))

(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 2.0)
  (which-key-idle-secondary-delay 1.0)
  :config
  (which-key-setup-side-window-right))

;; (use-package smerge-mode
;;   :config
;;   (smartrep-define-key
;;       smerge-mode-map "C-c ^"
;;     '(("n" . smerge-next)
;;       ("p" . smerge-prev)
;;       ("l" . smerge-keep-lower)
;;       ("u" . smerge-keep-upper)
;;       ("b" . smerge-keep-base)
;;       ("a" . smerge-keep-all)
;;       ("E" . smerge-ediff))))

(use-package winner
  :init
  (winner-mode 1)
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

;; (use-package hideif
;;   :init
;;   (add-hook 'c++-mode-hook 'hide-ifdef-mode)
;;   (add-hook 'c-mode-hook 'hide-ifdef-mode))

(use-package cperl-mode
  :mode (("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
  :init
  (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode)))

;; (define-generic-mode 'poe-filter-mode
;;   ;; Comments
;;   '("#")
;;   ;; Blocks
;;   '("Show" "Hide")
;;   '(
;;     ;; Conditions
;;     ("\\(ItemLevel\\|DropLevel\\|Quality\\|Rarity\\|Class\\|BaseType\\|Sockets\\|LinkedSockets\\|SocketGroup\\|Height\\|Width\\|HasExplicitMod\\|StackSize\\|GemLevel\\|Identified\\|Corrupted\\|ElderItem\\|ShaperItem\\|ElderMap\\|ShapedMap\\|MapTier\\)" . 'font-lock-variable-name-face)
;;     ;; Actions
;;     ("\\(SetBorderColor\\|SetTextColor\\|SetBackgroundColor\\|SetFontSize\\|PlayAlertSound\\|PlayAlertSoundPositional\\|DisableDropSound\\|CustomAlertSound\\|MinimapIcon\\|PlayEffect\\)" . 'font-lock-variable-name-face)
;;     ;; Attributes
;;     ("\\(True\\|False\\)" . 'font-lock-constant-face)
;;     ;; Rarity
;;     ("\\(Unique\\|Rare\\|Magic\\|Normal\\)" . 'font-lock-constant-face)
;;     ;; Color
;;     ("\\(Red\\|Green\\|Blue\\|Brown\\|White\\|Yellow\\)" . 'font-lock-constant-face)
;;     ;; Shape
;;     ("\\(Circle\\|Diamond\\|Hexagon\\|Square\\|Star\\|Triangle\\)" . font-lock-constant-face)
;;     ;; Beam
;;     ("\\(Temp\\)" . font-lock-constant-face)
;;     ;; Base Type
;;     ("\\(Jewel\\|Amulets\\|Belt\\|Ring\\|Wands\\|Daggers\\|One Hand\\|Shields\\|Thrusting\\|Sceptre\\|Claws\\|Currency\\|Gems\\|Flask\\|Maps\\|Piece\\)" . font-lock-constant-face)
;;     ;; Item Size
;;     ("\\(Small\\|Medium\\|Large\\)" . font-lock-constant-face)
;;     ;; Flask Tier
;;     ("\\(Greater\\|Grand\\|Giant\\|Colossal\\|Sacred\\|Hallowed\\|Sanctified\\|Divine\\|Eternal\\)" . font-lock-constant-face)
;;     )
;;   '(".filter\\'")
;;   nil
;;   "Major mode for editing Path of Exile filter file.")

(use-package json-ts-mode
  :mode
  (("\\.json\\'" . json-ts-mode))
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
  :custom
  (org-export-default-language "ja"))

(use-package ox-latex
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

(use-package tramp
  :demand t
  :custom
  (tramp-default-method "ssh"))

;; デフォルト色付け
(use-package generic-x)

(use-package java-ts-mode
  :mode
  (("\\.java\\'" . java-ts-mode)))

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(when (eq system-type 'gnu/linux)
;;; Fix copy/paste in Wayland
  ;; credit: yorickvP on Github
  (if (bound-and-true-p pgtk-initialized)
      (progn
        (defvar wl-copy-process nil)
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
        (setq interprogram-paste-function 'wl-paste)))
  ;; (use-package exec-path-from-shell
  ;;   :ensure t
  ;;   :custom
  ;;   (exec-path-from-shell-variables '("PATH" "MANPATH" "LSP_USE_PLISTS"))
  ;;   :config
  ;;   (exec-path-from-shell-initialize))

  ;; (use-package pdf-tools
  ;;  :ensure t
  ;;  :init
  ;;  (pdf-loader-install))
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
  ;; (use-package ispell
  ;;   :custom
  ;;   (ispell-program-name "hunspell")
  ;;   (ispell-dictionary "/usr/share/hunspell/en_US.dic")
  ;;   ;; (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  ;;   :config
  ;;   (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

  ;;   (use-package flyspell
  ;;     :config
  ;;     (unbind-key "C-M-i" flyspell-mode-map)
  ;;     (unbind-key "C-;" flyspell-mode-map)
  ;;     (unbind-key "C-," flyspell-mode-map))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (package-initialize)

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
  (super-save-auto-save-when-idle t)
  (super-save-hook-triggers '(mouse-leave-buffer-hook)))

(use-package tab-bar
  ;; tab-bar is built-in, but configs depend on extra packages
  ;; so put after package-initialize.
  :custom
  (tab-bar-show 1)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal global-map (kbd "T") 'tab-new)
    (evil-define-key 'normal global-map (kbd "C-S-t") 'tab-close)
    (evil-define-key 'normal global-map (kbd "L") 'tab-next)
    (evil-define-key 'normal global-map (kbd "H") 'tab-previous)
    (evil-define-key 'emacs dired-mode-map (kbd "T") 'tab-new)
    (evil-define-key 'emacs dired-mode-map (kbd "C-S-t") 'tab-close)
    (evil-define-key 'emacs dired-mode-map (kbd "L") 'tab-next)
    (evil-define-key 'emacs dired-mode-map (kbd "H") 'tab-previous)
    )
  (defun my/tab-bar-tab-name-format-with-icon (name tab i)
    "タブ名の前にnerd-iconsのアイコンを付与する。
この関数は`tab-bar-tab-name-format-functions`のフック関数として使用される。"
    (let* ((current-p (eq (car tab) 'current-tab))
           (buffer (if current-p
                       (current-buffer)
                     ;; 非カレントタブの場合はwindow-stateから最初のバッファを取得
                     (let* ((ws (alist-get 'ws tab))
                            (buffers (when ws (window-state-buffers ws)))
                            (buffer-name (car buffers)))
                       (when buffer-name
                         (get-buffer buffer-name)))))
           ;; アクティブタブは黒系、非アクティブタブは白系の色を使用
           (icon-color (if current-p
                           (doom-color 'bg)
                         (doom-color 'base6)))
           (icon-face (when icon-color
                        (list :foreground icon-color)))
           (icon (when buffer
                   (cond
                    ;; ファイルがある場合は拡張子でアイコンを決定
                    ((buffer-file-name buffer)
                     (if (fboundp 'nerd-icons-icon-for-file)
                         (nerd-icons-icon-for-file (buffer-file-name buffer) :face icon-face)
                       ""))
                    ;; dired-modeの場合
                    ((with-current-buffer buffer (derived-mode-p 'dired-mode))
                     (if (fboundp 'nerd-icons-octicon)
                         (nerd-icons-octicon "nf-oct-file_directory" :face icon-face)
                       ""))
                    ;; その他のバッファ
                    (t
                     (if (fboundp 'nerd-icons-icon-for-buffer)
                         (nerd-icons-icon-for-buffer :face icon-face)
                       ""))))))
      ;; アイコンがあれば名前の前に追加
      (if (and icon (not (string-empty-p icon)))
          (concat " " icon " " name " ")
        (concat " " name " "))))

  ;; tab-bar-tab-name-format-functionsの先頭にアイコン表示関数を追加
  (setq tab-bar-tab-name-format-functions
        (cons 'my/tab-bar-tab-name-format-with-icon
              tab-bar-tab-name-format-functions))

  (defun my/setup-tab-bar-faces ()
    (let ((tab-bg (doom-color 'dark-blue))
          (fg     (doom-color 'bg))
          (inactive-fg (doom-color 'base6)))
      (set-face-attribute 'tab-bar-tab nil
                          :background tab-bg
                          :foreground fg
                          :weight 'bold)
      (set-face-attribute 'tab-bar-tab-inactive nil
                          :background fg
                          :foreground inactive-fg)))
  (advice-add 'load-theme :after (lambda (&rest _) (my/setup-tab-bar-faces))))

(use-package doom-themes
  :ensure t
  :custom
  (window-divider-default-right-width 10)
  :hook
  (after-init . my/setup-doom-themes)
  :init
  (defun my/setup-doom-themes ()
    (load-theme 'doom-dracula t)
    (window-divider-mode 1)))

(use-package doom-modeline
  :ensure t
  :hook
  (emacs-startup . doom-modeline-mode)
  :commands (doom-modeline-def-modeline doom-modeline-def-segment)
  :config
  (defun remove-padding-zero (num)
    (if (string= (substring num 0 1) "0")
        (substring num 1)
      num))

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
    '(input-method bar modals matches remote-host buffer-info buffer-position csv-index)
    '(projectile-project-name vcs check battery datetime)
    )

  (doom-modeline-def-modeline 'verbose
    '(bar matches remote-host buffer-info-simple my-buffer-size)
    '(major-mode minor-modes python-venv buffer-encoding))

  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-checker-simple-format nil))

(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  :init
  ;; Evil compatibility fix
  (defun my/paredit-forward-visual-advice (orig-fn &rest args)
    "Advice for `paredit-forward' to move cursor back one char in visual state."
    (apply orig-fn args)
    (when (evil-visual-state-p)
      (backward-char 1)))

  (defun my/paredit-backward-advice (orig-fn &rest args)
    "Advice for `paredit-backward' to handle evil-mode cursor position.
Moves cursor forward before calling the original function when on a
closing delimiter in normal or visual state."
    (when (save-excursion (and (not (eobp)) (eq (char-syntax (char-after)) ?\))))
      (cond
       ((evil-visual-state-p)
        (forward-char 1))
       ((evil-normal-state-p)
        (if (eolp)
            (forward-line 1)
          (forward-char 1)))))
    (apply orig-fn args))
  (advice-add 'paredit-backward :around #'my/paredit-backward-advice)
  (advice-add 'paredit-forward :around #'my/paredit-forward-visual-advice))

;; (use-package enhanced-evil-paredit
;;   :ensure t
;;   :after (paredit))

(use-package posframe
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :init
  (defun my/evil-visual-mc-edit-lines ()
    "From evil visual mode, create multiple cursors on the selected lines.
For visual-line mode ('V'), places cursors at the beginning of each line.
For visual-char ('v') or visual-block ('C-v'), places cursors at the column."
    (interactive)
    (when (evil-visual-state-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (evil-exit-visual-state)
        (evil-emacs-state)
        (goto-char beg)
        ;; For char/block mode, make region inclusive by adjusting 'end'
        (if (and (not (eq evil-visual-selection 'line)) (> end beg))
            (setq end (1- end)))
        (push-mark end t t)
        ;; Use the correct function based on selection type
        (if (eq evil-visual-selection 'line)
            (mc/edit-beginnings-of-lines)
          (mc/edit-lines)))))

  (defun my/mc-finish-switch-to-evil-normal ()
    "When leaving multiple-cursors-mode, return to evil-normal-state."
    (when (eq evil-state 'emacs)
      (evil-normal-state)))

  (with-eval-after-load 'evil
    (evil-define-key 'visual global-map (kbd "gM") 'my/evil-visual-mc-edit-lines))
  :hook
  (multiple-cursors-mode-disabled . my/mc-finish-switch-to-evil-normal))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode +1)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/"))))

(use-package evil
  :ensure t
  :demand t
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
  (evil-swap-key evil-motion-state-map "k" "gk")
  (evil-define-key 'normal global-map (kbd "C-M-p") 'consult-yank-from-kill-ring))

;; (use-package evil-mode-line
;;   :init
;;   (let* ((site-lisp-dir "~/.emacs.d/elisp")
;;          (mode-line-color-file (concat site-lisp-dir "/mode-line-color.el"))
;;          (evil-mode-line-file (concat site-lisp-dir "/evil-mode-line.el")))
;;     (unless (and (file-exists-p mode-line-color-file) (file-exists-p evil-mode-line-file))
;;       (use-package url)
;;       (unless (file-directory-p site-lisp-dir)
;;         (mkdir site-lisp-dir))
;;       (url-copy-file "https://raw.githubusercontent.com/tarao/evil-plugins/master/evil-mode-line.el" evil-mode-line-file)
;;       (url-copy-file "https://raw.githubusercontent.com/tarao/elisp/master/mode-line-color.el" mode-line-color-file))
;;     (add-to-list 'load-path site-lisp-dir))
;;   :config
;;   (with-eval-after-load 'doom-themes
;;     (setopt evil-mode-line-color `((normal . ,(doom-color 'bg-alt))
;;                                    (insert . ,(doom-darken (doom-color 'green) 0.5))
;;                                    (visual . ,(doom-color 'dark-blue))
;;                                    (emacs . ,(doom-color 'magenta))))))
;; ;; (require 'evil-mode-line)

(use-package migemo
  :demand t
  :ensure t
  :custom
  (migemo-isearch-enable-p nil)
  (migemo-dictionary (seq-find #'file-exists-p
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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

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
  :commands (consult-ghq--list-candidates)
  :init
  (defun consult-ghq-root-dir ()
    "Directory switch to ghq project root dir."
    (interactive)
    (dired (consult--read (consult-ghq--list-candidates) :prompt "Repo: "))))

(use-package consult-jq
  :ensure t
  :vc (:url "https://github.com/bigbuger/consult-jq" :rev :newest))

(use-package embark-consult
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package ace-jump-mode
  :ensure t)

(use-package ace-isearch
  :ensure t
  :custom
  (ace-isearch-function 'ace-jump-char-mode)
  (ace-isearch-use-function-from-isearch nil)
  (ace-isearch-jump-delay 0.8)
  :init
  (global-ace-isearch-mode +1))

(use-package corfu
  :ensure t
  :after (evil)
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
  :commands (smartrep-define-key)
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

;; (use-package popwin
;;   :ensure t
;;   :init
;;   (popwin-mode 1))
(use-package shackle
  :ensure t
  :init (setq shackle-rules nil))

(use-package xref
  ;; :after (popwin)
  :init
  ;; (push '("*xref*" :position bottom :width 5)
  ;;       popwin:special-display-config)
  (add-to-list 'shackle-rules '("\\*xref\\*" :align 'below :size 5))
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  :bind
  (:map xref--xref-buffer-mode-map
        ("j" . xref-next-line)
        ("k" . xref-prev-line)))

(use-package flycheck
  :ensure t
  :pin melpa
  :custom
  (flyecheck-disabled-checkers 'python-ruff)
  ;; :init
  ;; (push '(flycheck-error-list-mode :position right :width 0.2 :noselect t :stick t)
  ;;       popwin:special-display-config)
  )


;; (use-package flymake
;;   :ensure t
;;   :pin gnu
;;   :after (popwin)
;;   :commands (flymake-show-buffer-diagnostics flymake-goto-next-error flymake-goto-prev-error)
;;   :bind (("C-c ! l" . flymake-show-buffer-diagnostics)
;;          ("C-c ! n" . flymake-goto-next-error)
;;          ("C-c ! p" . flymake-goto-prev-error))
;;   :config
;;   (smartrep-define-key
;;       flymake-mode-map "C-c !"
;;     '(("n" . flymake-goto-next-error)
;;       ("p" . flymake-goto-prev-error)))
;;   (push '(flymake-diagnostics-buffer-mode :position bottom :width 5 :noselect t)
;;         popwin:special-display-config))

;; ;; (use-package flymake-ruff
;; ;;   :ensure t
;; ;;   ;; :init
;; ;;   ;; (defun kle/flymake-ruff-load-python-ts-mode ()
;; ;;   ;;   "check major mode before load flymake-ruff"
;; ;;   ;;   (when (eq major-mode 'python-ts-mode)
;; ;;   ;;     (flymake-ruff-load)))
;; ;;   :hook
;; ;;   ;; (eglot-managed-mode . kle/flymake-ruff-load-python-ts-mode)
;; ;;   (python-mode . flymake-ruff-load)
;; ;;   (python-ts-mode . flymake-ruff-load)
;; ;;   )

(use-package highlight-indent-guides
  :ensure t
  :hook
  (python-mode . highlight-indent-guides-mode)
  (python-ts-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column))

;; (use-package pangu-spacing
;;   :ensure t
;;   :custom
;;   (pangu-spacing-real-insert-separtor t)
;;   :config
;;   ;; chinse-two-byte→japanese に置き換えて日本語で使う
;;   (setq pangu-spacing-chinese-before-english-regexp
;;         (rx (group-n 1 (category japanese))
;;             (group-n 2 (in "a-zA-Z0-9"))))
;;   (setq pangu-spacing-chinese-after-english-regexp
;;         (rx (group-n 1 (in "a-zA-Z0-9"))
;;             (group-n 2 (category japanese)))))

(use-package docker
  :ensure t)

(use-package python
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  ;; :init
  ;; (defun set-basedpyright-options ()
  ;;   (setq-default eglot-workspace-configuration
  ;;                 '(:basedpyright.analysis
  ;;                   ( :inlayHints ( :variableTypes nil
  ;;                                   :callArgumentNames nil
  ;;                                   :functionReturnTypes nil
  ;;                                   :genericTypes nil)))))
  :init
  (defun my/python-init-setup ()
    (setq tab-width python-indent-offset)
    (electric-operator-mode 1))
  :hook
  (python-ts-mode . my/python-init-setup)
  (python-mode . my/python-init-setup)
  :mode
  (("\\.py\\'" . python-ts-mode))
  ;; (python-ts-mode . set-basedpyright-options)
  ;; (python-mode . set-basedpyright-options)
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

;; (use-package eglot
;;   :ensure t
;;   :pin gnu
;;   :config
;;   (unless (require 'eglot-booster nil t)
;;     (package-vc-install "https://github.com/jdtsmith/eglot-booster"))
;;   (use-package eglot-booster
;;     :config (eglot-booster-mode +1))
;;   ;; (assq-delete-all '(python-mode python-ts-mode) eglot-server-programs)
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              '((python-mode python-ts-mode)
;;   ;;                "basedpyright-langserver" "--stdio"))
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              '((python-mode python-ts-mode)
;;   ;;                . ,(eglot-alternatives
;;   ;;                    '(("basedpyright-langserver" "--stdio")
;;   ;;                      "ruff-lsp"
;;   ;;                      "pylsp"
;;   ;;                      ("pyright-langserver" "--stdio")
;;   ;;                      "judi-language-server"
;;   ;;                      "pyls"))))
;;   (defun my-reorder-eldoc-functions ()
;;     "Ensure `flymake-eldoc-function` is the first in `eldoc-documentation-functions`."
;;     (when (and (boundp 'eldoc-documentation-functions)
;;                (listp eldoc-documentation-functions))
;;       (let ((flymake-fn 'flymake-eldoc-function))
;;         (setq eldoc-documentation-functions
;;               (cons flymake-fn (remove flymake-fn eldoc-documentation-functions))))))
;;   (add-hook 'eglot-managed-mode-hook #'my-reorder-eldoc-functions)
;;   (setq-default eglot-workspace-configuration
;;                 '(:yaml (:customTags ["!Sub scalar" "!Sub sequence" "!GetAtt scalar" "!Ref scalar"]))))

(use-package lsp-mode
  :ensure t
  ;; :hook (lsp-after-open . my-reorder-eldoc-functions)
  :custom
  ;; (lsp-disabled-clients '(lsp-ruff))
  ;; (lsp-log-io t)
  (lsp-diagnostics-provider :auto)
  (lsp-completion-provider :none)
  ;; (lsp-log-io t)
  ;; (lsp-auto-register-remote-clients nil)
  :init
  ;; (defun my-reorder-eldoc-functions ()
  ;;   "Ensure `flymake-eldoc-function` is the first in `eldoc-documentation-functions`."
  ;;   (when (and (boundp 'eldoc-documentation-functions)
  ;;              (listp eldoc-documentation-functions))
  ;;     (let ((flymake-fn 'flymake-eldoc-function))
  ;;       (setq eldoc-documentation-functions
  ;;             (cons flymake-fn (remove flymake-fn eldoc-documentation-functions))))))
  (add-to-list 'tramp-remote-path "/workspace/.venv/bin")
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :hook
  (lsp-mode . (lambda () (when (file-remote-p default-directory)
                           (setq-local lsp-enable-file-watchers nil))))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].aws-sam\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].claude\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].devcontainer\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].ruffcache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\].serena\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]data\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]db_schemas\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]rds_migration\\'")
  )
;; (setq lsp-pyright-langserver-command "/workspace/.venv/bin/basedpyright")
(use-package lsp-pyright
  :ensure t
  :hook
  ((python-mode python-ts-mode) . start-lsp-for-python)
  :init
  (defun start-lsp-for-python ()
    (require 'lsp-pyright)
    (lsp-deferred))
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  ;; disable basedpyright specific features
  (lsp-pyright-basedpyright-inlay-hints-variable-types nil)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names nil)
  (lsp-pyright-basedpyright-inlay-hints-function-return-types nil)
  (lsp-pyright-basedpyright-inlay-hints-generic-types nil)
  )

(use-package lsp-ruff
  :custom
  (lsp-ruff-log-level "debug"))


;; (with-eval-after-load 'lsp-pyright
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection
;;     (lsp-stdio-connection
;;      (lambda ()
;;        '("basedpyright-langserver" "--stdio")))
;;     :major-modes '(python-mode python-ts-mode)
;;     :server-id 'pyright-remote
;;     :multi-root lsp-pyright-multi-root
;;     :remote? t
;;     :priority 1
;;     :initialized-fn (lambda (workspace)
;;                       (with-lsp-workspace workspace
;;                                           (lsp--set-configuration
;;                                            (make-hash-table :test 'equal))))
;;     :notification-handlers (lsp-ht ((concat lsp-pyright-langserver-command "/beginProgress") 'lsp-pyright--begin-progress-callback)
;;                                    ((concat lsp-pyright-langserver-command "/reportProgress") 'lsp-pyright--report-progress-callback)
;;                                    ((concat lsp-pyright-langserver-command "/endProgress") 'lsp-pyright--end-progress-callback)))))

(use-package lsp-java
  :ensure t
  :hook (java-ts-mode . lsp-deferred)
  :custom
  (lsp-java-java-path "/usr/lib/jvm/java-21-openjdk-amd64/bin/java")
  :config
  (setq lsp-java-configuration-runtimes
        `[(:name "JavaSE-1.8" :path "/usr/lib/jvm/java-1.8.0-amazon-corretto" :default t)
          (:name "JavaSE-21"  :path "/usr/lib/jvm/java-21-openjdk-amd64")]))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (defun kle/lsp-ui-doc-dwim ()
    (interactive)
    (if (lsp-ui-doc--frame-visible-p)
        (lsp-ui-doc-hide)
      (lsp-ui-doc-show)))
  :bind
  (:map lsp-ui-mode-map
        ("C-l C-d" . kle/lsp-ui-doc-dwim))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; sidelineの日本語対応
  ;; 幅2の文字を考慮しておらず表示が崩れるので、関連している関数を全部書き直す
  (defun lsp-ui-sideline--make-display-string (info symbol current)
    "Make final string to display in buffer.
     INFO is the information to display.
     SYMBOL is the symbol associated with the info.
     CURRENT is non-nil when the point is on the symbol."
    (let* ((face (if current 'lsp-ui-sideline-current-symbol 'lsp-ui-sideline-symbol))
           (str (if lsp-ui-sideline-show-symbol
                    (concat info " " (propertize (concat " " symbol " ") 'face face))
                  info))
           (ch-len (length str))         ;; 文字数はプロパティ付与のために保持
           (vis-len (string-width str))  ;; 表示幅は string-width
           (margin (lsp-ui-sideline--margin-width)))
      (add-face-text-property 0 ch-len 'lsp-ui-sideline-global nil str)
      (concat
       (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align vis-len margin))))
       (propertize str 'display (lsp-ui-sideline--compute-height)))))

  ;; push-info: final-string の長さ（表示幅）を string-width で計算する
  (defun lsp-ui-sideline--push-info (win-width symbol bounds info bol eol)
    (let* ((markdown-hr-display-char nil)
           (info (or (alist-get info lsp-ui-sideline--cached-infos)
                     (-some--> (lsp:hover-contents info)
                       (lsp-ui-sideline--extract-info it)
                       (lsp-ui-sideline--format-info it win-width)
                       (progn (push (cons info it) lsp-ui-sideline--cached-infos) it))))
           (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
      (when (and info
                 (> (string-width info) 0)
                 (lsp-ui-sideline--check-duplicate symbol info))
        (let* ((visible (if lsp-ui-sideline-show-symbol
                            (concat info " " (concat " " symbol " "))
                          info))
               (vis-w (string-width visible))
               (final-string (lsp-ui-sideline--make-display-string info symbol current))
               (pos-ov (lsp-ui-sideline--find-line vis-w bol eol))
               (ov (when pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
          (when pos-ov
            (overlay-put ov 'info info)
            (overlay-put ov 'symbol symbol)
            (overlay-put ov 'bounds bounds)
            (overlay-put ov 'current current)
            (overlay-put ov 'after-string final-string)
            (overlay-put ov 'before-string " ")
            (overlay-put ov 'window (get-buffer-window))
            (overlay-put ov 'kind 'info)
            (overlay-put ov 'position (car pos-ov))
            (push ov lsp-ui-sideline--ovs))))))

  ;; diagnostics: msg の幅を string-width で使う
  (defun lsp-ui-sideline--diagnostics (buffer bol eol)
    "Show diagnostics belonging to the current line."
    (when (and (bound-and-true-p flycheck-mode)
               (bound-and-true-p lsp-ui-sideline-mode)
               lsp-ui-sideline-show-diagnostics
               (eq (current-buffer) buffer))
      (lsp-ui-sideline--delete-kind 'diagnostics)
      (dolist (e (flycheck-overlay-errors-in bol (1+ eol)))
        (let* ((lines (--> (flycheck-error-format-message-and-id e)
                           (split-string it "\n")
                           (lsp-ui-sideline--split-long-lines it)))
               (display-lines (butlast lines (- (length lines) lsp-ui-sideline-diagnostic-max-lines)))
               (offset 1))
          (dolist (line (nreverse display-lines))
            (let* ((msg (string-trim (replace-regexp-in-string "[\t ]+" " " line)))
                   (msg (replace-regexp-in-string " " " " msg))
                   (ch-len (length msg))
                   (w-len (string-width msg))
                   (level (flycheck-error-level e))
                   (face (if (eq level 'info) 'success level))
                   (margin (lsp-ui-sideline--margin-width))
                   (msg (progn (add-face-text-property 0 ch-len 'lsp-ui-sideline-global nil msg)
                               (add-face-text-property 0 ch-len face nil msg)
                               msg))
                   (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align w-len margin))))
                                   (propertize msg 'display (lsp-ui-sideline--compute-height))))
                   (pos-ov (lsp-ui-sideline--find-line w-len bol eol t offset))
                   (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
              (when pos-ov
                (setq offset (1+ (car (cdr pos-ov))))
                (overlay-put ov 'after-string string)
                (overlay-put ov 'kind 'diagnostics)
                (overlay-put ov 'before-string " ")
                (overlay-put ov 'position (car pos-ov))
                (push ov lsp-ui-sideline--ovs))))))))

  ;; code-actions: タイトル幅を string-width で計算、画像は幅1とみなす
  (defun lsp-ui-sideline--code-actions (actions bol eol)
    "Show code ACTIONS."
    (let ((inhibit-modification-hooks t))
      (when lsp-ui-sideline-actions-kind-regex
        (setq actions (seq-filter (-lambda ((&CodeAction :kind?))
                                    (or (not kind?)
                                        (s-match lsp-ui-sideline-actions-kind-regex kind?)))
                                  actions)))
      (setq lsp-ui-sideline--code-actions actions)
      (lsp-ui-sideline--delete-kind 'actions)
      (seq-doseq (action actions)
        (-let* ((title (->> (lsp:code-action-title action)
                            (replace-regexp-in-string "[\n\t ]+" " ")
                            (replace-regexp-in-string " " " ")
                            (concat (unless lsp-ui-sideline-actions-icon
                                      lsp-ui-sideline-code-actions-prefix))))
                (image (lsp-ui-sideline--code-actions-image action))
                (margin (lsp-ui-sideline--margin-width))
                (keymap (let ((map (make-sparse-keymap)))
                          (define-key map [down-mouse-1] (lambda () (interactive)
                                                           (save-excursion
                                                             (lsp-execute-code-action action))))
                          map))
                (ch-len (length title))
                (w-len (string-width title))
                (img-w (if image 1 0))
                (title (progn (add-face-text-property 0 ch-len 'lsp-ui-sideline-global nil title)
                              (add-face-text-property 0 ch-len 'lsp-ui-sideline-code-action nil title)
                              (add-text-properties 0 ch-len `(keymap ,keymap mouse-face highlight) title)
                              title))
                (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align (+ w-len img-w) margin))))
                                image
                                (propertize title 'display (lsp-ui-sideline--compute-height))))
                (pos-ov (lsp-ui-sideline--find-line (+ 1 w-len img-w) bol eol t))
                (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
          (when pos-ov
            (overlay-put ov 'after-string string)
            (overlay-put ov 'before-string " ")
            (overlay-put ov 'kind 'actions)
            (overlay-put ov 'position (car pos-ov))
            (push ov lsp-ui-sideline--ovs)))))
    )
  )



(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-python)
  (dap-auto-configure-mode 1)
  (setq dap-python-debugger 'debugpy))

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
(use-package lv :ensure t)
(use-package dired
  :custom
  ;; fix keybind for SKK
  (dired-bind-jump nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :init
  (evil-set-initial-state 'dired-mode 'emacs)
  :config
  (bind-keys :map dired-mode-map
             ("C-t" . other-window-or-split)
             ("j" . dired-next-line)
             ("k" . dired-previous-line))
  (when (eq system-type 'gnu/linux)
    (setopt dired-listing-switches "-AFDlh --group-directories-first"))
  (when (eq system-type 'windows-nt)
    (setopt ls-lisp-dirs-first t)))

(use-package dired-x
  :after (dired)
  :custom
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..+$")
  :bind (:map dired-mode-map
              ("C-l C-o" . dired-omit-mode)))

(use-package wdired
  :after (dired evil)
  :init
  (defun kle/wdired-evil-fix ()
    "Evil fix for wdired. Stay Normal mode when entering WDired."
    (progn
      (evil-normal-state)
      (forward-char)))
  (add-hook 'wdired-mode-hook #'kle/wdired-evil-fix)
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))

(use-package dired-quick-sort
  :ensure t
  :after (dired)
  :commands (hydra-dired-quick-sort/body)
  :init
  (bind-key "S" 'hydra-dired-quick-sort/body dired-mode-map))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; (use-package direx
;;   :ensure t
;;   :init
;;   (defun kle/direx-open ()
;;     (interactive)
;;     (or (ignore-errors
;;           (direx-project:jump-to-project-root-other-window))
;;         (direx:jump-to-directory-other-window)))
;;   (defun kle/direx-dwim ()
;;     (interactive)
;;     (if (derived-mode-p 'direx:direx-mode)
;;         (kill-buffer)
;;       (kle/direx-open)))
;;   :bind
;;   (("<f8>" . kle/direx-dwim))
;;   :after (popwin)
;;   :config
;;   (push '(direx:direx-mode :position left :width 40 :dedicated t)
;;         popwin:special-display-config)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "j") 'direx:next-item)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "k") 'direx:previous-item)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "J") 'direx:next-sibling-item)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "K") 'direx:previous-sibling-item)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "^") 'direx:up-item)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "RET") 'direx:maybe-find-item)
;;   (evil-define-key 'normal direx:direx-mode-map (kbd "TAB") 'direx:toggle-item))

(use-package neotree
  :ensure t
  :init
  (evil-set-initial-state 'neotree-mode 'emacs)
  :bind
  (("<f8>" . neotree-toggle))
  (:map neotree-mode-map
        ("j" . neotree-next-line)
        ("k" . neotree-previous-line)
        ("C-t" . other-window-or-split))
  :custom
  (neo-theme 'nerd-icons)
  (neo-smart-open t)
  (neo-force-change-root t)
  (neo-autorefresh t)
  (neo-window-fixed-size t)
  (neo-show-hidden-files t)
  (neo-vc-integration '(face)))

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

;; (use-package image+
;;   :ensure t
;;   :after (image)
;;   :config
;;   (defhydra imagex-sticky-binding (global-map "C-l i")
;;     "Manipulating Image"
;;     ("+" imagex-sticky-zoom-in "zoom in")
;;     ("-" imagex-sticky-zoom-out "zoom out")
;;     ("M" imagex-sticky-maximize "maximize")
;;     ("O" imagex-sticky-restore-original "restore original")
;;     ("S" imagex-sticky-save-image "save file")
;;     ("r" imagex-sticky-rotate-right "rotate right")
;;     ("l" imagex-sticky-rotate-left "rotate left")))

(use-package imenu-list
  :ensure t
  :custom
  ;; (imenu-list-auto-resize t)
  (imenu-list-position 'left)
  (imenu-list-size 0.18)
  ;; :hook
  ;; (imenu-list-after-jump . imenu-list-smart-toggle)
  :bind
  ("C-;" . imenu-list-smart-toggle)
  :config
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "j") 'next-line)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "k") 'previous-line)
  (evil-define-key 'normal imenu-list-major-mode-map (kbd "RET") 'imenu-list-goto-entry))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-l m s" . magit-status)
         ("C-l m l c" . magit-log-current)
         ("C-l m l b" . magit-log-buffer-file))
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :config
  (defun surpress-iconify (&rest arg)
    (remove-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  (defun apply-iconify (&rest arg)
    (add-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  (advice-add 'magit-run-git-with-editor :before #'surpress-iconify)
  (advice-add 'with-editor-finish :after #'apply-iconify)
  ;; (push '(magit-status-mode :position right :width 0.5)
  ;;       popwin:special-display-config)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package anzu
  :ensure t
  :pin melpa
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
  (backward-forward-mode 1)
  :config
  (setq backward-forward-evil-compatibility-mode t)
  (advice-add 'evil-goto-first-line :before #'backward-forward-push-mark-wrapper)
  (advice-add 'evil-goto-line :before #'backward-forward-push-mark-wrapper)
  :bind
  (:map backward-forward-mode-map
        ("C-l C-a" . backward-forward-previous-location)
        ("C-l C-f" . backward-forward-next-location)))

(use-package electric-operator
  :ensure t)

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
  :ensure t)

(use-package google-translate
  :ensure t
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
  (use-package google-translate-smooth-ui))

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
  :bind (("C-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :pin melpa
  :after yasnippet)

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.js\\'" . web-mode)))

(use-package typescript-mode
  :ensure t)

(use-package tuareg
  :ensure t
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)
         ("\\.mly\\'" . tuareg-mode)
         ("\\.mll\\'" . tuareg-mode)
         ("\\.mlp\\'" . tuareg-mode)))

(use-package powershell
  :ensure t
  :mode (("\\.ps1'" . powershell-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode))
  :init
  (defun my/disable-ispell-capf ()
    "completion-at-point-functions から ispell を除外する。"
    (setq-local completion-at-point-functions
                (remove #'ispell-completion-at-point
                        completion-at-point-functions)))
  :hook (gfm-mode . my/disable-ispell-capf)
  :custom
  (markdown-command "multimarkdown")
  (markdown-italic-underscore t)
  :config
  (defconst markdown-regex-italic
    "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:[_]\\)\\(?3:[^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)[^\\ ]\\)\\(?4:\\2\\)\\)")
  (defconst markdown-regex-gfm-italic
    "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:[_]\\)\\(?3:[^ \\]\\2\\|[^ ]\\(?:.\\|\n[^\n]\\)\\)\\(?4:\\2\\)\\)")
  (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css")))

(use-package markdown-preview-mode
  :ensure t)

(use-package rust-mode
  :ensure t
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
            (cadr latex-list) "%l")))
  )
;;   :config
;;   (TeX-source-correlate-mode +1)
;;   (use-package pdf-sync)
;;   (bind-key "C-c s s" 'pdf-sync-forward-search LaTeX-mode-map)
;;   (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
;;     (delq (assoc command TeX-command-list) TeX-command-list))
;;   (LaTeX-math-mode +1)
;;   (turn-on-reftex)

(use-package vimrc-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
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
  :ensure t)

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

;; (use-package csv-mode
;;   :ensure t
;;   :init
;;   ;; (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))
;;   ;; (add-to-list 'auto-mode-alist '("\\.tsv\\'" . tsv-mode))
;;   (defun enable-csv-mode-for-small-files ()
;;     "Enable csv-mode for TSV/CSV files if they are not too large."
;;     (when (and buffer-file-name
;;                (or (string-match-p "\\.csv\\'" buffer-file-name)
;;                    (string-match-p "\\.tsv\\'" buffer-file-name))
;;                (or (not large-file-warning-threshold)
;;                    (< (buffer-size) large-file-warning-threshold)))
;;       (csv-mode)))
;;   (add-hook 'find-file-hook 'enable-csv-mode-for-small-files)
;;   (defun kle/smartrep-csv-setup ()
;;     (smartrep-define-key
;;         csv-mode-map "C-c" '(("l" . csv-forward-field)
;;                              ("h" . csv-backward-field))))
;;   :hook
;;   (csv-mode . csv-align-mode)
;;   (tsv-mode . csv-align-mode)
;;   (csv-mode . (lambda () (toggle-truncate-lines t)))
;;   (tsv-mode . (lambda () (toggle-truncate-lines t)))
;;   (csv-mode . kle/smartrep-csv-setup)
;;   :bind
;;   (:map csv-mode-map
;;         ("C-c l" . csv-forward-field)
;;         ("C-c h" . csv-backward-field))
;;   :custom
;;   (csv-align-style 'auto)
;;   (csv-align-max-width 200))

(use-package yaml-pro
  :ensure t
  :hook (yaml-mode . yaml-pro-mode))

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
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map (kbd "j") 'dashboard-next-line)
    (evil-define-key 'normal dashboard-mode-map (kbd "k") 'dashboard-previous-line)
    (evil-define-key 'normal dashboard-mode-map (kbd "r") 'dashboard-jump-to-recent-files)))

(use-package ligature
  :ensure t
  :defer 1
  :hook
  (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 'prog-mode
                          '("->" "<-" "=>" "=>>" ">=>" "=>=" "=<<" "=<=" "<=<" "<=>"
                            ">>" ">>>" "<<" "<<<" "<>" "<|>" "==" "===" ".=" ":="
                            "#=" "!=" "!==" "=!=" "=:=" "::" ":::" ":<:" ":>:"
                            "||" "|>" "||>" "|||>" "<|" "<||" "<|||"
                            ;; "**" "***"
                            "<*" "<*>" "*>" "<+" "<+>" "+>" "<$" "<$>" "$>"
                            "$$" "%%" "|]" "[|")))

(use-package gptel
  :ensure t
  :pin melpa
  :custom
  (gptel-api-key (getenv "OPENAI_API_KEY"))
  (gptel-model 'gpt-5.1)
  ;; ;; Mark gptel-related file-local variables as safe.
  ;; (put 'gptel-model 'safe-local-variable
  ;;      (lambda (v) (or (symbolp v) (stringp v))))
  ;; (put 'gptel--backend-name 'safe-local-variable #'stringp)
  ;; (put 'gptel--bounds 'safe-local-variable #'listp)

  ;; (defcustom gptel-save-directory "~/gptel/"
  ;;   "Directory where gptel chat buffers are saved by `gptel-save-as-md'."
  ;;   :type 'directory
  ;;   :group 'gptel)
  ;; (bind-key "C-l c o" #'gptel-open-saved-chat)
  ;; :config
  ;; (bind-key "C-x C-s" #'gptel-save-buffer-dwim gptel-mode-map)
  )

(use-package gptel-bedrock
  :after gptel
  :config
  (add-to-list 'gptel-bedrock--model-ids
               '(claude-sonnet-4-5-20250929 . "jp.anthropic.claude-sonnet-4-5-20250929-v1:0"))
  (gptel-make-bedrock "AWS"
    :stream t
    :region "ap-northeast-1"
    :models '(claude-sonnet-4-5-20250929)
    :model-region 'apac
    ;; :aws-profile 'sinops-corporate
    :aws-bearer-token (getenv "AWS_BEARER_TOKEN_BEDROCK")))

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install)
  :config
  (setopt gptel-magit-commit-prompt
          "You are an expert software engineer and meticulous code reviewer.
Your task is to generate a single Git commit message that **strictly follows the Conventional Commits v1.0.0 Specification**.

### PRIMARY GOAL

Produce one short, complete commit message for the staged changes.

---

### SPEC FOR YOUR REFERENCE

Conventional Commits 1.0.0
==========================

Summary
=======

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history; which makes it
easier to write automated tools on top of. This convention dovetails with [SemVer](http://semver.org/),
by describing the features, fixes, and breaking changes made in commit messages.

Structure
=========

The commit message should be structured as follows:
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

Core Elements
============

The commit contains the following structural elements, to communicate intent to the consumers of your library:

1. **fix:** a commit of the _type_`fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
2. **feat:** a commit of the _type_`feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
3. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning). A BREAKING CHANGE can be part of commits of any _type_.
4. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`, `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
5. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE). A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.

Type Definitions
==============

Each commit type has a specific meaning and purpose:

- **fix**: A commit that patches a bug in your codebase
- **feat**: A commit that introduces a new feature to the codebase
- **build**: Changes that affect the build system or external dependencies
- **chore**: Changes to the build process or auxiliary tools and libraries
- **ci**: Changes to CI configuration files and scripts
- **docs**: Documentation only changes
- **perf**: A code change that improves performance
- **refactor**: A code change that neither fixes a bug nor adds a feature
- **style**: Changes that do not affect the meaning of the code
- **test**: Adding missing tests or correcting existing tests

Note: Types other than \"fix:\" and \"feat:\" are allowed and have no implicit effect in
semantic versioning (unless they include a BREAKING CHANGE).

Detailed Rules
=============

The key words \"MUST\", \"MUST NOT\", \"REQUIRED\", \"SHALL\", \"SHALL NOT\", \"SHOULD\", \"SHOULD NOT\", \"RECOMMENDED\", \"MAY\", and \"OPTIONAL\" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

1. Commits MUST be prefixed with a type, which consists of a noun, `feat`, `fix`, etc., followed by the OPTIONAL scope, OPTIONAL `!`, and REQUIRED terminal colon and space.
2. The type `feat` MUST be used when a commit adds a new feature to your application or library.
3. The type `fix` MUST be used when a commit represents a bug fix for your application.
4. A scope MAY be provided after a type. A scope MUST consist of a noun describing a section of the codebase surrounded by parenthesis, e.g., `fix(parser):`
5. A description MUST immediately follow the colon and space after the type/scope prefix. The description is a short summary of the code changes, e.g., _fix: array parsing issue when multiple spaces were contained in string_.
6. A longer commit body MAY be provided after the short description, providing additional contextual information about the code changes. The body MUST begin one blank line after the description.
7. A commit body is free-form and MAY consist of any number of newline separated paragraphs.
8. One or more footers MAY be provided one blank line after the body. Each footer MUST consist of a word token, followed by either a `:<space>` or `<space>#` separator, followed by a string value (this is inspired by the [git trailer convention](https://git-scm.com/docs/git-interpret-trailers)).
9. A footer's token MUST use `-` in place of whitespace characters, e.g., `Acked-by` (this helps differentiate the footer section from a multi-paragraph body). An exception is made for `BREAKING CHANGE`, which MAY also be used as a token.
10. A footer's value MAY contain spaces and newlines, and parsing MUST terminate when the next valid footer token/separator pair is observed.
11. Breaking changes MUST be indicated in the type/scope prefix of a commit, or as an entry in the footer.
12. If included as a footer, a breaking change MUST consist of the uppercase text BREAKING CHANGE, followed by a colon, space, and description, e.g., _BREAKING CHANGE: environment variables now take precedence over config files_.
13. If included in the type/scope prefix, breaking changes MUST be indicated by a `!` immediately before the `:`. If `!` is used, `BREAKING CHANGE:` MAY be omitted from the footer section, and the commit description SHALL be used to describe the breaking change.
14. Types other than `feat` and `fix` MAY be used in your commit messages, e.g., _docs: update ref docs._
15. The units of information that make up Conventional Commits MUST NOT be treated as case sensitive by implementors, with the exception of BREAKING CHANGE which MUST be uppercase.
16. BREAKING-CHANGE MUST be synonymous with BREAKING CHANGE, when used as a token in a footer.

### Examples

#### Commit message with description and breaking change footer
```
feat: allow provided config object to extend other configs

BREAKING CHANGE: `extends` key in config file is now used for extending other config files
```

#### Commit message with `!` to draw attention to breaking change
```
feat!: send an email to the customer when a product is shipped
```

#### Commit message with scope and `!` to draw attention to breaking change
```
feat(api)!: send an email to the customer when a product is shipped
```

#### Commit message with both `!` and BREAKING CHANGE footer
```
chore!: drop support for Node 6

BREAKING CHANGE: use JavaScript features not available in Node 6.
```

#### Commit message with no body
```
docs: correct spelling of CHANGELOG
```

#### Commit message with scope
```
feat(lang): add Polish language
```

#### Commit message with multi-paragraph body and multiple footers
```
fix: prevent racing of requests

Introduce a request id and a reference to latest request. Dismiss
incoming responses other than from latest request.

Remove timeouts which were used to mitigate the racing issue but are
obsolete now.

Reviewed-by: Z
Refs: #123
```

### OUTPUT FORMAT
- Return **only** the commit message text—no code fences, no commentary, no extra markup or explanations.
- The summary (first) line **must** be imperative, present tense, ≤72 characters, and **must not** end with a period.
- Wrap all body lines at a maximum of 72 characters.
- If a body is included, format it as a clean, concise bullet list, each line starting with - .

### OUTPUT LANGUAGE
-- Use Japanese for the commit message.
-- Type keywords like feat or fix should not be translated into Japanese. Use as is.

---

")
  (setopt gptel-magit-diff-explain-prompt
          (concat gptel-magit-diff-explain-prompt
                  " Use Japanase for the answer."))
  (defun my-gptel-magit--format-commit-message (message)
    message)
  (advice-add 'gptel-magit--format-commit-message :override
              #'my-gptel-magit--format-commit-message)
  (setq gptel-magit-model 'gpt-4.1-mini))

(use-package minuet
  :ensure t
  :custom
  (minuet-provider 'openai)
  (minuet-request-timeout 10)
  (minuet-n-completions 1)
  :bind (:map minuet-active-mode-map
              ("<tab>" . 'minuet-accept-suggestion)
              ("TAB" . 'minuet-accept-suggestion)
              ("C-<left>" . 'minuet-previous-suggestion)
              ("C-<right>" . 'minuet-next-suggestion))
  ;; :hook (python-ts-mode . minuet-auto-suggestion-mode)
  )

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-@" . claude-code-ide-menu)
  :custom
  (claude-code-ide-diagnostics-backend 'flycheck)
  :config
  ;; dedicated buffer for editing prompt
  (defvar claude-code-ide-prompt-buffer-close-after-send t)
  (defvar claude-code-ide-prompt-focus-claude-after-send t)

  (defvar claude-code-ide-prompt-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-s") #'claude-code-ide-prompt-send-and-close)
      (define-key map (kbd "C-c C-c") #'claude-code-ide-prompt-send)
      (define-key map (kbd "C-c C-k") (lambda () (interactive) (kill-buffer (current-buffer))))
      map)
    "Keymap for claude-code-ide-prompt-mode.")

  (define-minor-mode claude-code-ide-prompt-mode
    "Minor mode for editing Claude prompts."
    :init-value nil
    :lighter " Claude-Prompt"
    :keymap claude-code-ide-prompt-mode-map)

  (defun claude-code-ide--current-project-root ()
    (if-let ((proj (ignore-errors (project-current))))
        (expand-file-name (project-root proj))
      (expand-file-name default-directory)))

  (defun claude-code-ide-open-prompt-buffer ()
    "現在プロジェクト用のプロンプト編集バッファを開く。"
    (interactive)
    (let* ((root (claude-code-ide--current-project-root))
           (proj (file-name-nondirectory (directory-file-name root)))
           (buf (get-buffer-create (format "*Claude Prompt: %s*" proj))))
      (with-current-buffer buf
        ;; 送信先の判定に使うので default-directory をプロジェクトに合わせる
        (setq default-directory root)
        ;; markdown-mode があれば使う（任意）
        (when (require 'markdown-mode nil t)
          (markdown-mode))
        (claude-code-ide-prompt-mode 1)
        (setq-local header-line-format "C-c C-c: 送信, C-c C-s: 送信+閉じる, C-c C-k: 閉じる"))
      (pop-to-buffer buf)))

  (defun claude-code-ide--prompt-collect ()
    "送信するテキストを取得（リージョンがあればリージョン、なければバッファ全体）。"
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max))))

  (defun claude-code-ide-prompt-send ()
    "編集バッファの内容を送信（バッファは残す）。"
    (interactive)
    (let ((text (claude-code-ide--prompt-collect)))
      (condition-case err
          (progn
            (claude-code-ide-send-prompt text)
            (when claude-code-ide-prompt-focus-claude-after-send
              (claude-code-ide-switch-to-buffer))
            (message "Claudeにプロンプトを送信しました"))
        (error
         (message "送信失敗: %s" (error-message-string err))))))

  (defun claude-code-ide-prompt-send-and-close ()
    "編集バッファの内容を送信して（成功時）バッファを閉じる。"
    (interactive)
    (let ((buf (current-buffer)))
      (call-interactively #'claude-code-ide-prompt-send)
      (when (and claude-code-ide-prompt-buffer-close-after-send
                 (buffer-live-p buf))
        (kill-buffer buf))))

  ;; ;; キーに割り当て
  ;; (define-prefix-command 'claude-code-ide-prefix)
  ;; (global-set-key (kbd "C-c c") 'claude-code-ide-prefix)
  ;; (global-set-key (kbd "C-c c p") #'claude-code-ide-open-prompt-buffer)

  ;; Transientメニューに項目を追加（任意）
  (with-eval-after-load 'claude-code-ide-transient
    (when (fboundp 'transient-append-suffix)
      (transient-append-suffix 'claude-code-ide-menu "p"
        '("P" "Open prompt editor" claude-code-ide-open-prompt-buffer))))

  ;; enable Emacs specific tools
  (claude-code-ide-emacs-tools-setup))

(use-package emojify
  :ensure t
  :hook
  (text-mode . global-emojify-mode)
  (prog-mode . global-emojify-mode)
  :custom
  (emojify-emoji-styles '(unicode github)))

;;; Linux specific setup
(when (eq system-type 'gnu/linux)
  (use-package vterm
    :ensure t
    :custom
    (vterm-max-scrollback 100000)
    :config
    ;; 例外キーに追加
    (add-to-list 'vterm-keymap-exceptions "C-t")
    ;; 既に作られているキーマップに反映
    (when (boundp 'vterm-mode-map)
      (vterm--exclude-keys vterm-mode-map vterm-keymap-exceptions)))
  (use-package vterm-toggle
    :ensure t
    :bind
    (("<f10>" . vterm-toggle))
    (:map vterm-mode-map
          ("<f10>" . vterm-toggle)))
  (use-package eat
    :ensure t
    ;; :init
    ;; (add-hook 'eat-mode-hook
    ;;       (lambda ()
    ;;         (add-hook 'pre-command-hook
    ;;                   (lambda ()
    ;;                     (when (and (boundp 'eat-terminal) eat-terminal
    ;;                                (boundp 'eat--line-mode) eat--line-mode
    ;;                                (< (point) (eat-term-end eat-terminal))
    ;;                                (let ((s (symbol-name this-command)))
    ;;                                  (or (string-prefix-p "skk-" s)
    ;;                                      (eq this-command 'self-insert-command))))
    ;;                       (goto-char (point-max))))
    ;;                   nil t)))
    )
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
  ;; (use-package treesit-auto
  ;;   :ensure t
  ;;   :init
  ;;   (add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter/")
  ;;   :commands
  ;;   (global-treesit-auto-mode)
  ;;   :hook
  ;;   (prog-mode . global-treesit-auto-mode)
  ;;   :custom
  ;;   (treesit-auto-install 'nil)
  ;;   :config
  ;;   (treesit-auto-add-to-auto-mode-alist 'all))
  (use-package treesit-fold
    :init
    (let* ((elpa-lisp-dir "~/.emacs.d/elpa")
           (treesit-fold-file (concat elpa-lisp-dir "/treesit-fold/treesit-fold.el")))
      (unless (file-exists-p treesit-fold-file)
        (package-vc-install "https://github.com/emacs-tree-sitter/treesit-fold")))
    :bind
    ("C-l o" . treesit-fold-toggle))

  (use-package autodisass-java-bytecode
    :ensure t)
  )
