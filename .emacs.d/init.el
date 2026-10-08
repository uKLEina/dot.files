;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setopt custom-file (locate-user-emacs-file "custom.el"))
(setopt initial-major-mode 'fundamental-mode)

;;; OS判定
;; Windows (非WSL) では開発をしないため、テキスト読み書きに必要な最小構成のみ読み込む。
;; 開発系・外部バイナリ依存・重量級の外部パッケージは use-package の :if my/linux-p で無効化する。
(defconst my/windows-p (eq system-type 'windows-nt))
(defconst my/linux-p (eq system-type 'gnu/linux))

;;; Windows固有の調整
(when my/windows-p
  (setq w32-get-true-file-attributes nil)   ; ファイル属性の詳細取得をやめてファイル操作を軽くする
  (setq inhibit-compacting-font-caches t)   ; 日本語フォントのキャッシュ圧縮によるGC停滞を防ぐ
  (setq ring-bell-function #'ignore)        ; w32のvisible-bellは画面全体が点滅してうるさい
  (setq w32-pipe-buffer-size (* 64 1024))   ; 外部プロセス出力(git等)の読み取りを速くする
  ;; 起動直後のカレントディレクトリがemacs.exeの場所(C:/Program Files/...)になるのを防ぐ
  (setq default-directory (expand-file-name "~/"))
  (setq command-line-default-directory (expand-file-name "~/"))
  ;; .emacs.d/cmigemo/ にcmigemo一式(exe+dll+dict)を置けばPATHを通さなくても使えるようにする
  (let ((cmigemo-dir (expand-file-name (locate-user-emacs-file "cmigemo"))))
    (when (file-directory-p cmigemo-dir)
      (add-to-list 'exec-path cmigemo-dir))))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-unset-key (kbd "C-l"))
(defun my/server-edit-save-and-done ()
  "Save buffer and finish editing for emacsclient without confirmation."
  (interactive)
  (save-buffer)
  (server-edit))

(bind-keys ("C-l C-l" . recenter-top-bottom)
           ("C-l C-x" . my/server-edit-save-and-done)
           ("C-l C-<tab>" . tab-to-tab-stop)
           ("C-M-y" . duplicate-dwim))

;;; 基本設定のセットアップ
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
(defvar my/backup-dir (locate-user-emacs-file "backup/"))
(make-directory my/backup-dir t)
(setopt backup-directory-alist `((".*" . ,my/backup-dir)))
(setopt auto-save-file-name-transforms `((".*" ,my/backup-dir t)))
(setq auto-save-timeout 15)
(setq auto-save-interval 60)
;; treesitはgrammarのビルドにコンパイラが要るのでLinuxのみ
(when my/linux-p
  (setopt treesit-auto-install-grammar 'always)
  (setopt treesit-enabled-modes t))

(defun my/setup-modes ()
  "各種モードの有効化"
  (auto-compression-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (pixel-scroll-precision-mode 1)
  (global-auto-revert-mode 1)
  (global-subword-mode 1))
(add-hook 'after-init-hook #'my/setup-modes)

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

(use-package whitespace
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tab-mark trailing))
  (whitespace-display-mappings '((tab-mark ?\t [?▸ ?\t])))
  :custom-face
  (whitespace-trailing ((t (:background "gray" :inherit nil))))
  :config
  ;; 全角スペースの可視化
  (defface my/whitespace-full-width-space '((t (:background "medium aquamarine"))) nil)
  (defun my/add-full-width-space-highlight ()
    (font-lock-add-keywords nil '(("\u3000" 0 'my/whitespace-full-width-space append))))
  (add-hook 'font-lock-mode-hook #'my/add-full-width-space-highlight))

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
  (let ((utility-buffers '("*Ilist*" "*Flycheck errors*")))
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
                   (not (window-live-p previous-win))           ; 直前のウィンドウが削除済みなら気にしない
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
  (if-let* ((file-path (buffer-file-name)))
      (let ((text-to-copy (if use-file-name-only
                              (file-name-nondirectory file-path)
                            file-path)))
        (kill-new text-to-copy)
        (message "Copied: %s" text-to-copy))
    (message "This buffer is not associated with a file")))

(defun copy-project-buffer-file-path ()
  (interactive)
  (let* ((project-root (file-local-name (abbreviate-file-name
                                         (or (when-let* ((project (project-current)))
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
           (when-let* ((relative-path (file-relative-name
                                       (or (file-name-directory buffer-file-name)
                                           "./")
                                       project-root)))
             (if (string= relative-path "./")
                 ""
               relative-path))
           ;; File name
           (file-name-nondirectory buffer-file-name))))
    (kill-new project-buffer-file-path)
    (message "copied: %s" project-buffer-file-path)))

;; (setopt debug-on-error t)

;;; font
;; (set-face-attribute 'default nil :family "Monaspace Neon" :height 130)
;; (set-face-attribute 'default nil :family "HackGen" :height 140)
;; (set-face-attribute 'default nil :family "IBM Plex Mono" :height 130)
;; (set-face-attribute 'default nil :family "Ricty Discord" :height 120)
;; (set-face-attribute 'default nil :family "0xProto" :height 110)
;; (set-face-attribute 'default nil :family "Monaspace Radon" :height 130) ;; :D
;; (set-face-attribute 'default nil :family "Cascadia Code" :height 105)
;; non-ASCII Unicode font
;; (set-fontset-font t '(#x80 . #x10ffff) (font-spec :family "Noto Mono" :size 10))
;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Noto Sans Mono" :size 50))
;; (set-fontset-font t nil (font-spec :family "Noto Sans" :size 100))
(setq use-default-font-for-symbols nil)

(defvar my/font-candidates
  (if my/windows-p
      '("ProtoGen" "HackGen" "BIZ UDGothic" "MS Gothic")
    '("ProtoGen" "HackGen"))
  "使いたい順のフォント候補。最初に見つかったものを使う。")
(defvar my/font-family
  (or (and (display-graphic-p)
           (seq-find (lambda (f) (find-font (font-spec :family f)))
                     my/font-candidates))
      (car my/font-candidates)))
(defvar my/font-height (if my/windows-p 110 140)
  "要求するデフォルトのフォント高さ（:height の単位）。")
(set-face-attribute 'default nil :family my/font-family :height my/font-height)
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

;; Windowsでは use-package の ensure を全面無効化し、使うパッケージのみ明示的に入れる。
;; use-package の :ensure と :pin は :if が nil でもマクロ展開時に発火する仕様のため、
;; :if my/linux-p だけでは開発系パッケージのインストールを防げない。
;; :pin は package-archives を参照するので、先に package を読み込んでおく。
(when my/windows-p
  (require 'package)
  (require 'use-package)
  (setq use-package-ensure-function #'ignore)
  (defvar my/windows-packages
    '(evil undo-tree multiple-cursors expreg                  ; 編集の基本操作
      vertico orderless marginalia consult                    ; ミニバッファ補完
      corfu cape kind-icon                                    ; バッファ内補完
      ddskk migemo                                            ; 日本語入力・検索
      doom-themes doom-modeline dashboard diminish            ; 見た目
      nerd-icons nerd-icons-completion nerd-icons-dired
      ligature emojify
      posframe shackle                                        ; ウィンドウ・UI部品
      markdown-mode yaml-mode powershell                      ; メジャーモード
      paredit smartparens rainbow-delimiters                  ; 括弧・入力支援
      electric-operator anzu
      highlight-symbol highlight-indent-guides backward-forward
      magit forge gptel gptel-magit                           ; Git・AI
      dired-quick-sort                                        ; dired
      super-save open-junk-file)                              ; 自動保存・メモ
    "Windows環境でインストールする外部パッケージ。
consult-jq は :vc (git経由) でインストールされるためこのリストには含めない。")
  (let ((missing (seq-remove #'package-installed-p my/windows-packages)))
    (when missing
      (package-refresh-contents)
      (dolist (pkg missing)
        (package-install pkg)))))

;;; configure built-in packages before package-initialize
(use-package server
  :init
  (when (and (not (boundp 'pgtk-initialized)) (eq system-type 'gnu/linux) (window-system))
    (defun raise-frame-with-wmctrl (&optional frame)
      (call-process "wmctrl" nil nil nil "-i" "-R"
                    (frame-parameter (or frame (selected-frame)) 'outer-window-id)))
    (advice-add 'raise-frame :after #'raise-frame-with-wmctrl))
  (defvar my/claude-code-frame nil
    "Dedicated GUI frame for Claude Code-originated emacsclient sessions.
Lazily created on the first edit and reused via make-frame-invisible /
make-frame-visible so we never destroy/recreate the GTK widget — that
avoids the new-frame focus-in use-after-free crash on WSLg.")
  (defvar my/claude-code-session-active nil
    "Non-nil while a Claude Code-originated emacsclient session is active.")
  (defvar my/claude-code-pending nil
    "Non-nil when next emacsclient session is from Claude Code.")
  (defun my/claude-code-show-frame ()
    "Bring the dedicated Claude Code frame forward, creating it if needed.
On pgtk/Wayland an unmap+remap roundtrip is used to bypass Mutter's
focus-stealing prevention so the frame actually comes to the front."
    (if (and my/claude-code-frame (frame-live-p my/claude-code-frame))
        (progn
          (make-frame-invisible my/claude-code-frame)
          (make-frame-visible my/claude-code-frame))
      (setq my/claude-code-frame
            (make-frame '((name . "claude-code"))))))
  (defun my/claude-code-server-setup ()
    "Move Claude Code session into the dedicated frame; enable Evil + SKK."
    (when my/claude-code-pending
      (setq my/claude-code-pending nil
            my/claude-code-session-active t)
      (let ((buf (current-buffer)))
        (my/claude-code-show-frame)
        ;; Keep the main frame's working buffer intact
        (switch-to-prev-buffer)
        (select-frame-set-input-focus my/claude-code-frame)
        (switch-to-buffer buf))
      (goto-char (point-max))
      (evil-insert-state)
      (skk-mode 1)))
  (defun iconify-emacs-when-server-is-done ()
    (cond
     (server-clients)
     (my/claude-code-session-active
      (setq my/claude-code-session-active nil)
      (when (and my/claude-code-frame (frame-live-p my/claude-code-frame))
        ;; make-frame-invisible だとフォーカスの受け渡しがコンポジタ任せになり、
        ;; 本体フレームにフォーカスが飛ぶことがある。iconify-frame は最小化の意図が
        ;; WM に伝わるため、直前のウィンドウ (Windows Terminal 側) に戻りやすい。
        (iconify-frame my/claude-code-frame)))
     (t (iconify-frame))))
  (defun my/server-visit-setup-keybindings ()
    "Setup C-c C-c to save and finish in emacsclient buffers."
    (local-set-key (kbd "C-c C-c") #'my/server-edit-save-and-done))
  (add-hook 'server-switch-hook #'raise-frame)
  (add-hook 'server-switch-hook #'my/claude-code-server-setup)
  (add-hook 'server-visit-hook #'my/server-visit-setup-keybindings)
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

(use-package winner
  :hook
  (after-init . winner-mode)
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

(use-package cperl-mode
  :mode (("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
  :init
  (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode)))

(use-package json-ts-mode
  :if my/linux-p
  :mode
  (("\\.json\\'" . json-ts-mode))
  :custom (json-ts-mode-indent-offset 2))

;;; 案件をまたいだタスク管理（~/Project/task-agenda の README.md）
(defvar my/project-root (expand-file-name "~/Project/")
  "案件のディレクトリを置く場所。案件のタスクリストは <案件>/tasks.org。")
(defvar my/task-agenda-dir (expand-file-name "task-agenda/" my/project-root)
  "案件をまたいだタスク管理のリポジトリ。")

(defun my/org-project-task-files ()
  "各案件のタスクリスト。"
  (file-expand-wildcards (expand-file-name "*/tasks.org" my/project-root)))

(defun my/org-agenda-update-files (&rest _)
  "案件が増えても agenda に入るよう、org-agenda-files を作り直す。
Claude Code が書いた :ID: も :BLOCKED_BY: のリンク先として見つかるよう、ID の場所も読み直す。"
  (setq org-agenda-files
        (append (my/org-project-task-files)
                (list (expand-file-name "inbox.org" my/task-agenda-dir))
                (file-expand-wildcards (expand-file-name "external/*.org" my/task-agenda-dir))))
  (when (featurep 'org)
    (require 'org-id)
    (org-id-update-id-locations org-agenda-files t)))

(defun my/task-agenda-run (name &optional on-exit)
  "task-agenda の bin/NAME を裏で動かす。出力があればメッセージに出す。
動いている間は、もう1つは動かさない。ON-EXIT は終わったときに呼ぶ。"
  (unless (process-live-p (get-process name))
    (let ((buf (get-buffer-create (format " *%s*" name))))
      (with-current-buffer buf (erase-buffer))
      (make-process
       :name name :buffer buf :noquery t
       :command (list (expand-file-name (concat "bin/" name) my/task-agenda-dir))
       :sentinel (lambda (proc _event)
                   (unless (process-live-p proc)
                     (let ((out (string-trim (with-current-buffer (process-buffer proc) (buffer-string)))))
                       (unless (string-empty-p out) (message "%s: %s" name out)))
                     (when on-exit (funcall on-exit))))))))

(defun my/agenda-backup-after-save ()
  "案件のタスクリストを保存したら、projects/ にコピーして commit する。"
  (when (and buffer-file-name
             (string-match-p (concat "\\`" (regexp-quote my/project-root) "[^/]+/tasks\\.org\\'")
                             (file-truename buffer-file-name)))
    (my/task-agenda-run "agenda-backup")))
(add-hook 'after-save-hook #'my/agenda-backup-after-save)

(defun my/agenda-mirror ()
  "外部トラッカーの写しを取り直し、終わったら開いている agenda を描き直す。"
  (interactive)
  (my/task-agenda-run
   "agenda-mirror"
   (lambda ()
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (derived-mode-p 'org-agenda-mode)
           (org-agenda-redo t)))))))
(defvar my/agenda-mirror-interval (* 30 60)
  "外部トラッカーの写しを取り直す間隔（秒）。")
(add-hook 'emacs-startup-hook
          (lambda () (run-with-timer 60 my/agenda-mirror-interval #'my/agenda-mirror)))

;; agenda の行の頭の %-12(...) は文字数で揃えるので、日本語が入るとずれる。
;; 行の頭に出す関数は、見た目の幅で揃えた文字列を返す
(defun my/org-agenda-pad (s width &optional right)
  "S を見た目の幅 WIDTH に揃える。長ければ「…」で切る。RIGHT なら右寄せ。"
  ;; 全角の途中で切ると WIDTH に届かないことがあり、切ったあとは埋めてくれないので、自分で埋める
  (let* ((s (truncate-string-to-width (or s "") width nil nil "…"))
         (pad (make-string (max 0 (- width (string-width s))) ?\s)))
    (if right (concat pad s) (concat s pad))))

(defun my/org-agenda-waiting-on ()
  "agenda の行の頭に出す、待ち相手（:WAITING_ON:）。"
  (my/org-agenda-pad (org-entry-get nil "WAITING_ON") 14))

(defun my/org-state-days (state)
  "今の見出しが STATE になってからの日数。
いちばん新しい状態のメモ（State \"STATE\"）の日付から数える。メモが無ければ nil。"
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (or (outline-next-heading) (point-max)))))
      ;; 状態のメモは新しいものが上（org-log-states-order-reversed）なので、最初に見つかったものを使う
      (when (re-search-forward
             (concat "^[ \t]*- State \"" (regexp-quote state)
                     "\"[ \t]+from .*?\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\)\\]")
             end t)
        (- (org-today) (time-to-days (org-time-string-to-time (match-string 1))))))))

(defun my/org-agenda-waiting-days ()
  "agenda の行の頭に出す、待ちになってからの日数。メモが無ければ空白。"
  (my/org-agenda-pad (let ((d (my/org-state-days "WAIT"))) (and d (format "%d日" d))) 5 t))

(defun my/org-agenda-format-date (date)
  "agenda の日付の行。system-time-locale が \"C\" だと曜日が英語になるので、自分で書く。"
  (format "%d年%d月%d日（%s）" (nth 2 date) (car date) (nth 1 date)
          (aref ["日" "月" "火" "水" "木" "金" "土"] (calendar-day-of-week date))))

(defun my/org-agenda-waited-by ()
  "agenda の行の頭に出す、自分を待っている人（:WAITED_BY:）と期限。"
  (let ((deadline (org-entry-get nil "DEADLINE")))
    (concat (my/org-agenda-pad (org-entry-get nil "WAITED_BY") 14) " "
            (my/org-agenda-pad (and deadline (format-time-string "%m/%d" (org-time-string-to-time deadline))) 5))))

(defun my/dashboard-open-agenda ()
  "まとめた画面を開く。"
  (interactive)
  (org-agenda nil "d"))

(defun my/dashboard-insert-today (_list-size)
  "dashboard の Today's Agenda の欄。まとめた画面のうち、朝イチに要る所だけを出す。
待たせているものと今日の予定は1件ずつ、期限切れ・待ち・inbox は件数だけ。
dashboard のほかの欄に合わせて、言葉は英語にする。"
  (require 'org)
  (dashboard-insert-heading "Today's Agenda:" "a")
  (insert "\n")
  (condition-case err
      (let ((today (org-today))
            (inbox-file (expand-file-name "inbox.org" my/task-agenda-dir))
            waited scheduled (overdue 0) (waiting 0) (inbox 0))
        (my/org-agenda-update-files)
        (org-map-entries
         (lambda ()
           (unless (or (null (org-get-todo-state)) (org-entry-is-done-p))
             (let* ((state (org-get-todo-state))
                    (sched (org-entry-get nil "SCHEDULED"))
                    (sd (and sched (time-to-days (org-time-string-to-time sched))))
                    (dl (org-entry-get nil "DEADLINE"))
                    (dd (and dl (time-to-days (org-time-string-to-time dl))))
                    (who (org-entry-get nil "WAITED_BY"))
                    (cat (my/org-agenda-pad (org-get-category) 12))
                    (title (org-link-display-format (org-get-heading t t t t))))
               (when who
                 (push (list (or dd most-positive-fixnum)
                             (concat cat " " (my/org-agenda-pad who 14) " "
                                     (my/org-agenda-pad (and dl (format-time-string "%m/%d" (org-time-string-to-time dl))) 5)
                                     "  " title))
                       waited))
               ;; まとめた画面の今日の予定と同じく、待ちは件数だけにする
               (cond
                ((equal state "WAIT") (setq waiting (1+ waiting)))
                ((or (eql sd today) (eql dd today))
                 (let ((label (cond ((and (eql sd today) (string-match "[0-9]+:[0-9]+" sched)) (match-string 0 sched))
                                    ((eql dd today) "Due")
                                    (t "Sched"))))
                   ;; 並べる順の鍵: 時刻のあるものは時刻順、そのあと期限、予定
                   (push (list (cond ((string-match-p ":" label) (concat "0" label)) ((equal label "Due") "1") (t "2"))
                               (concat cat " " (my/org-agenda-pad label 6) " " title))
                         scheduled)))
                ((or (and sd (< sd today)) (and dd (< dd today))) (setq overdue (1+ overdue))))
               (when (file-equal-p (buffer-file-name) inbox-file) (setq inbox (1+ inbox))))))
         nil 'agenda)
        (insert "  Waiting on me\n")
        (dolist (w (or (sort waited (lambda (a b) (< (car a) (car b)))) '((nil "None"))))
          (insert "    " (cadr w) "\n"))
        (insert "  Today\n")
        (dolist (e (or (sort scheduled (lambda (a b) (string< (car a) (car b)))) '((nil "None"))))
          (insert "    " (cadr e) "\n"))
        (insert (format "  Also: %d overdue / %d waiting / %d in inbox\n" overdue waiting inbox)))
    (error (insert (format "  Could not build Today's Agenda: %s\n" (error-message-string err))))))

(defun my/org-blocked-by-ids ()
  "今の見出しの :BLOCKED_BY: に書いた、待っているタスクの ID の一覧。"
  (let ((v (or (org-entry-get nil "BLOCKED_BY") "")) (start 0) ids)
    (while (string-match "\\[\\[id:\\([^]]+\\)\\]" v start)
      (push (match-string 1 v) ids)
      (setq start (match-end 0)))
    (nreverse ids)))

(defun my/org-blocked-by-done-p (change)
  "org-blocker-hook に足す関数。:BLOCKED_BY: のリンク先が1つでも済んでいなければ、DONE にさせない。
agenda でブロックされているものを薄く出すのも、この判定を使う。
リンク先が見つからないときは止めない。"
  (let ((to (plist-get change :to)))
    (if (not (and (eq (plist-get change :type) 'todo-state-change)
                  (or (eq to 'done) (member to org-done-keywords))))
        t
      (save-excursion
        (goto-char (plist-get change :position))
        (seq-every-p
         (lambda (id)
           (let ((m (org-id-find id 'marker)))
             (or (null m)
                 (prog1 (with-current-buffer (marker-buffer m)
                          (save-excursion (goto-char m) (org-entry-is-done-p)))
                   (set-marker m nil)))))
         (my/org-blocked-by-ids))))))

(defvar my/project-watch-days 3
  "待ち・着手中がこの日数以上続いたら、気にすべきことに出す。期限が近い未着手も、この日数以内を出す。")

(defvar-local my/project-watch-file nil
  "気にすべきことの画面が見ている tasks.org。")

(defun my/project-watch--file ()
  "agenda の行か今のバッファから、案件の tasks.org を決める。
外部トラッカーの写しの行なら、同じ案件名の tasks.org にする。"
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (let* ((marker (or (org-get-at-bol 'org-hd-marker) (org-get-at-bol 'org-marker)))
           (category (org-get-at-bol 'org-category))
           (f (and category (expand-file-name (concat category "/tasks.org") my/project-root))))
      (cond ((and f (file-exists-p f)) f)
            (marker (buffer-file-name (marker-buffer marker)))
            (t (user-error "この行には案件が無い")))))
   ((derived-mode-p 'my/project-watch-mode) my/project-watch-file)
   ((and buffer-file-name (derived-mode-p 'org-mode)) buffer-file-name)
   (t (user-error "agenda か org のバッファで呼ぶ"))))

(defun my/project-watch--collect (file)
  "FILE の、済んでいないタスクのうち気にすべきものを集める。アーカイブとレーンの外（概要）は見ない。
返すのは ((項目 . 行の一覧) ...)。行は (マーカー レーン 印 見出し)。"
  (let* ((today (org-today)) (n my/project-watch-days) found lane
         (add (lambda (key mark text) (push (list (point-marker) lane mark text) (alist-get key found)))))
    (progn
      (with-current-buffer (find-file-noselect file)
        (org-map-entries
         (lambda ()
           (let ((state (org-get-todo-state))
                 (title (org-link-display-format (org-get-heading t t t t))))
             (when (= (org-current-level) 1)
               (setq lane (unless (equal title "アーカイブ") title)))
             (when (and lane state (not (org-entry-is-done-p)) (not (member "someday" (org-get-tags))))
               (let* ((dl (org-entry-get nil "DEADLINE"))
                      (dd (and dl (- (time-to-days (org-time-string-to-time dl)) today)))
                      (sc (org-entry-get nil "SCHEDULED"))
                      (sd (and sc (- today (time-to-days (org-time-string-to-time sc)))))
                      (who (org-entry-get nil "WAITED_BY")))
                 ;; 期限と予定日は、重い方にだけ出す（期限切れ → 期限が近い未着手 → 予定日を過ぎた）
                 (cond ((and dd (< dd 0)) (funcall add 'overdue (format "期限%d日超過" (- dd)) title))
                       ((and dd (equal state "TODO") (<= dd n))
                        (funcall add 'due-soon (if (= dd 0) "今日が期限" (format "期限まで%d日" dd)) title))
                       ((and sd (> sd 0)) (funcall add 'slipped (format "予定%d日前" sd) title)))
                 (when (equal state "WAIT")
                   (let ((d (my/org-state-days "WAIT")))
                     (when (and d (>= d n))
                       (funcall add 'long-wait (format "%d日 %s" d (or (org-entry-get nil "WAITING_ON") "")) title))))
                 (when (equal state "NOW")
                   (let ((d (my/org-state-days "NOW")))
                     (when (and d (>= d n)) (funcall add 'stale-now (format "%d日" d) title))))
                 (when who
                   (funcall add 'waited-by (concat who (if dl (format-time-string " %m/%d" (org-time-string-to-time dl)) "")) title))))))
         nil 'file)))
    (mapcar (lambda (c) (cons (car c) (nreverse (cdr c)))) found)))

(defun my/project-watch--render ()
  "気にすべきことの画面を書き直す。"
  (let* ((inhibit-read-only t)
         (found (my/project-watch--collect my/project-watch-file))
         (n my/project-watch-days)
         (sections `((overdue . "期限切れ")
                     (due-soon . ,(format "期限が近い未着手（%d日以内）" n))
                     (slipped . "予定日を過ぎた")
                     (long-wait . ,(format "長い待ち（%d日以上）" n))
                     (stale-now . ,(format "止まっている着手中（%d日以上）" n))
                     (waited-by . "人を待たせている")))
         (cat (with-current-buffer (find-file-noselect my/project-watch-file) (org-get-category (point-min))))
         ;; 列の幅は、出ている中でいちばん長いものに合わせる（切らずに揃える）
         (items (apply #'append (mapcar #'cdr found)))
         (lane-w (apply #'max 0 (mapcar (lambda (it) (string-width (nth 1 it))) items)))
         (mark-w (apply #'max 0 (mapcar (lambda (it) (string-width (nth 2 it))) items))))
    (erase-buffer)
    (insert (format "%s で気にすべきこと\n" cat))
    (dolist (sec sections)
      (let ((items (alist-get (car sec) found)))
        (insert (format "\n%s%s\n" (cdr sec) (if items (format "（%d）" (length items)) "")))
        (if (null items)
            (insert "  特になし\n")
          (dolist (it items)
            (insert (propertize (concat "  " (my/org-agenda-pad (nth 1 it) lane-w) "  "
                                        (my/org-agenda-pad (nth 2 it) mark-w) "  " (nth 3 it))
                                'my/marker (nth 0 it))
                    "\n")))))
    (goto-char (point-min))))

(defun my/project-watch-visit ()
  "気にすべきことの画面の行のタスクへ飛ぶ。"
  (interactive)
  ;; 行のどこにカーソルがあっても飛べるよう、行の頭の飛び先を見る
  (let ((m (get-text-property (line-beginning-position) 'my/marker)))
    (unless m (user-error "この行には飛び先が無い"))
    (pop-to-buffer (marker-buffer m))
    (goto-char m)
    (org-fold-reveal t)
    (org-fold-show-entry)))

(define-derived-mode my/project-watch-mode special-mode "気にすべきこと"
  "案件の tasks.org の、気にすべきタスクの一覧。RET で飛ぶ、g で見直す、q で閉じる。"
  (setq-local revert-buffer-function (lambda (&rest _) (my/project-watch--render))))
(keymap-set my/project-watch-mode-map "RET" #'my/project-watch-visit)

(defun my/project-watch ()
  "agenda の行（か今のバッファ）の案件で、気にすべきタスクを横に出す。
期限切れ・期限が近い未着手・予定日を過ぎたもの・長い待ち・止まっている着手中・人を待たせているもの。"
  (interactive)
  (let* ((file (my/project-watch--file))
         (buf (get-buffer-create (format "*気にすべきこと: %s*" (file-name-nondirectory (directory-file-name (file-name-directory file)))))))
    (with-current-buffer buf
      (my/project-watch-mode)
      (setq my/project-watch-file file)
      (my/project-watch--render))
    (pop-to-buffer buf)))

(use-package org
  :bind
  ("C-l C-o l" . org-store-link)
  ("C-l C-o a" . org-agenda)
  ("C-l C-o c" . org-capture)
  :init
  ;; タイムスタンプの曜日を英語で書く。Claude Code や batch の Emacs が書くものと揃える
  (setq system-time-locale "C")
  :custom
  (org-latex-packages-alist
   '(("" "fontspec" t)
     ("" "xeCJK" t)))
  (org-todo-keywords
   ;; NOW は日時を記録する（気にすべきことの「止まっている着手中」で日数を数える）
   '((sequence "TODO(t)" "NOW(n!)" "WAIT(w@)" "|" "DONE(d)" "CANCELED(c@)")))
  ;; :ORDERED: の付いたまとまりは、手前の子が済むまで後ろの子を DONE にさせない
  (org-enforce-todo-dependencies t)
  ;; 別ファイル（tasks.org_archive）にすると、共有リポジトリに入りうる・agenda-backup が
  ;; コピーしない・clocktable の範囲から外れるので、同じファイルの見出しに移す
  (org-archive-location "::* アーカイブ")
  (org-capture-templates
   `(("i" "inbox" entry (file ,(expand-file-name "inbox.org" my/task-agenda-dir))
      "* TODO %?\n%U")))
  (org-refile-targets '((my/org-project-task-files :maxlevel . 2)))
  ;; どの案件も tasks.org なので、ファイル名ではなくパスで見分ける
  (org-refile-use-outline-path 'full-file-path)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  :config
  (add-hook 'org-blocker-hook #'my/org-blocked-by-done-p)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t))))

(my/org-agenda-update-files)
(advice-add 'org-agenda :before #'my/org-agenda-update-files)

(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ("M" . my/agenda-mirror)
              ("o" . my/project-watch))
  :custom
  ;; 画面の言葉を日本語にそろえる。行の頭の言葉は、見た目の幅を12桁にそろえる
  (org-agenda-format-date #'my/org-agenda-format-date)
  (org-agenda-scheduled-leaders '("予定        " "予定 %2d日前 "))
  (org-agenda-deadline-leaders '("期限        " "期限まで%2d日" "期限%2d日超過"))
  (org-agenda-current-time-string "← 今")
  (org-agenda-custom-commands
   ;; 朝イチ・作業の切れ目・休憩中に開いて、全体を掴んで次を決める画面。大事な順に並べる
   '(("d" "まとめた画面"
      ((tags-todo "WAITED_BY<>\"\""
                  ((org-agenda-overriding-header "待たせているもの")
                   (org-agenda-prefix-format "  %-14:c%(my/org-agenda-waited-by) ")
                   (org-agenda-sorting-strategy '(deadline-up category-keep))))
       (agenda "" ((org-agenda-span 'day)
                   (org-agenda-overriding-header "今日の予定")
                   ;; 待ちは下の「待ち」のブロックに日数つきで出るので、ここでは出さない
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("WAIT")))))
       ;; 人を待たせているものは、いちばん上の「待たせているもの」にだけ出す
       (todo "NOW" ((org-agenda-overriding-header "着手中")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":WAITED_BY:"))))
       (todo "WAIT" ((org-agenda-overriding-header "待ち")
                     (org-agenda-prefix-format "  %-14:c%(my/org-agenda-waiting-days) %(my/org-agenda-waiting-on) ")))
       (alltodo "" ((org-agenda-overriding-header "inbox に残っているもの")
                    (org-agenda-files (list (expand-file-name "inbox.org" my/task-agenda-dir))))))))))

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
  :custom
  (tramp-default-method "ssh"))

;; デフォルト色付け
(use-package generic-x
  :demand t)

(use-package java-ts-mode
  :if my/linux-p
  :mode
  (("\\.java\\'" . java-ts-mode)))

(use-package treesit
  :if my/linux-p
  :custom
  (treesit-font-lock-level 4))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-l h" . hs-toggle-hiding)
         ("C-l H" . my-hs-toggle-all))
  :init
  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun my-hs-toggle-all ()
    "Toggle hideshow all."
    (interactive)
    (setq my-hs-hide (not my-hs-hide))
    (if my-hs-hide (hs-hide-all) (hs-show-all))))

(use-package js
  :if my/linux-p
  :mode (("\\.js\\'" . js-ts-mode)))

(use-package typescript-ts-mode
  :if my/linux-p
  :mode
  (("\\.ts\\'" . typescript-ts-mode)))

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
(use-package auto-compile
  :if my/linux-p
  :ensure t
  :hook
  (emacs-startup . auto-compile-on-load-mode)
  (emacs-startup . auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  :config
  (advice-add 'auto-compile-byte-compile :around
              (lambda (orig-fn &rest args)
                (unless (equal (file-name-nondirectory (buffer-file-name)) "init.el")
                  (apply orig-fn args)))))

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

  (doom-modeline-def-segment project-name
    "Display project name via project.el"
    (let ((proj (project-current)))
      (if proj
          (propertize (format " [%s]" (project-name proj))
                      'face (if (doom-modeline--active)
                                '(:foreground "#8cd0d3" :weight bold)
                              'mode-line-inactive))
        "")))

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
  (doom-modeline-def-modeline 'simple
    '(input-method bar modals matches remote-host buffer-info buffer-position csv-index)
    '(project-name vcs check battery datetime))

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
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist `(("." . ,(locate-user-emacs-file "undo-tree-history/")))))

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :custom
  (evil-echo-state nil)
  (evil-undo-system 'undo-tree)
  :init
  ;; Emacs 31 で `define-globalized-minor-mode' の実装が変わり、
  ;; `evil-mode-buffers' を自動生成しなくなった (evil-core.el は前方宣言のみ)。
  ;; evil-1.15.0 の `evil-initializing-p' がこの変数を参照するので、
  ;; void-variable エラーを防ぐため明示的に定義しておく。
  (defvar evil-mode-buffers nil)
  (defun evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  :config
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

;; cmigemoバイナリと辞書が見つかった時だけ有効化する。
;; 見つからない環境 (未セットアップのWindows等) では素のisearch (C-s/C-r) のまま。
(defvar my/migemo-dictionary
  (seq-find #'file-exists-p
            `("/usr/share/cmigemo/utf-8/migemo-dict"
              ,(expand-file-name "~/opt/migemo/dict/utf-8/migemo-dict")
              ;; Windows: 配布バイナリのzipを展開して置く想定の場所
              ,(expand-file-name "~/opt/cmigemo/dict/utf-8/migemo-dict")
              ;; locate-user-emacs-file は "~" 付きの省略パスを返すが、
              ;; このパスは外部プログラムのcmigemoに渡すため絶対パスに展開しておく
              ,(expand-file-name
                (locate-user-emacs-file "cmigemo/dict/utf-8/migemo-dict"))))
  "最初に見つかったmigemo辞書。nilならmigemoは無効。")

(use-package migemo
  :if (and (executable-find "cmigemo") my/migemo-dictionary)
  :ensure t
  :custom
  (migemo-isearch-enable-p nil)
  (migemo-dictionary my/migemo-dictionary)
  :init
  ;; migemo は遅延ロードなので、コンパイル時に special 変数と認識させるための前方宣言。
  ;; :init はここに書いた通りの順でトップレベルへ直接展開される (deferされない) ので、
  ;; ここに置けば下の kle/isearch-forward-migemo 等の let による一時的な動的束縛が
  ;; lexical-binding 下でバイトコンパイルしても効くようになる。
  (defvar migemo-isearch-enable-p)
  ;; C-u で migemo を有効にする isearch
  (defun kle/isearch-forward-migemo (arg)
    "通常は通常のisearch。C-uでmigemoが有効になる。"
    (interactive "P")
    (unless (featurep 'migemo)
      (require 'migemo)
      (migemo-init))
    (let ((migemo-isearch-enable-p arg))
      (isearch-forward)))
  (defun kle/isearch-backward-migemo (arg)
    "通常は通常のisearch。C-uでmigemoが有効になる(後方検索)。"
    (interactive "P")
    (unless (featurep 'migemo)
      (require 'migemo)
      (migemo-init))
    (let ((migemo-isearch-enable-p arg))
      (isearch-backward)))
  (bind-key "C-s" 'kle/isearch-forward-migemo)
  (bind-key "C-r" 'kle/isearch-backward-migemo))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

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
  :hook (after-init . marginalia-mode))

(use-package embark
  :if my/linux-p
  :ensure t
  :bind
  (("<backtab>" . embark-act)))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("M-i" . consult-line-thing-at-point)
   ("C-M-g" . consult-ripgrep)
   ("M-g g" . consult-goto-line)
   )
  :custom
  (consult-async-min-input 2)
  :config
  ;; cosult-line-thing-at-point
  (consult-customize consult-line :add-history (seq-some #'thing-at-point '(region symbol)))
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize consult-line-thing-at-point :initial (thing-at-point 'symbol)))

(use-package consult-ghq
  :if my/linux-p
  :ensure t
  :commands (consult-ghq--list-candidates)
  :init
  (defun consult-ghq-root-dir ()
    "Directory switch to ghq project root dir."
    (interactive)
    (require 'consult)
    (dired (consult--read (consult-ghq--list-candidates) :prompt "Repo: "))))

(use-package consult-jq
  :ensure t
  :vc (:url "https://github.com/bigbuger/consult-jq" :rev :newest))

(use-package embark-consult
  :if my/linux-p
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package wgrep
  :if my/linux-p
  :ensure t)

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
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil)
    (evil-define-key 'insert corfu-map (kbd "C-n") 'corfu-next)
    (evil-define-key 'insert corfu-map (kbd "C-p") 'corfu-previous)))

(use-package cape
  :ensure t
  :bind (("C-:" . my-cape-dabbrev-completion))
  :custom
  (cape-dabbrev-buffer-function #'my-cape-project-buffers)
  (project-vc-extra-root-markers '(".project"))
  :config
  (defun my-cape-dabbrev-completion ()
    "必要な補完関数のみで補完する"
    (interactive)
    (let ((completion-at-point-functions '(cape-keyword cape-file cape-dabbrev)))
      (completion-at-point)))

  (defun my-cape-project-buffers ()
    "現在のバッファがプロジェクト内なら同じプロジェクトの全バッファ、
  そうでなければ同じメジャーモードのバッファを返す"
    (if-let* ((proj (project-current)))
        ;; プロジェクト内: project.elの標準関数を使う
        (project-buffers proj)
      ;; プロジェクト外: 同じメジャーモードのバッファ
      (cape-same-mode-buffers))))

;; cape-keywordを有効化（autoloadのため）
(use-package cape-keyword
  :ensure nil
  :after cape)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package repeat
  :init
  (repeat-mode 1)
  :config
  (defvar-keymap window-resize-repeat-map
    :repeat t
    "}" #'enlarge-window-horizontally
    "{" #'shrink-window-horizontally
    "o" #'other-window)

  (defvar-keymap tab-stop-repeat-map
    :repeat t
    "<tab>" #'tab-to-tab-stop))

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode))

(use-package xref
  :init
  (with-eval-after-load 'shackle
    (add-to-list 'shackle-rules '("*xref*" :align below :size 0.3)))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'xref--xref-buffer-mode 'emacs))
  :bind
  (:map xref--xref-buffer-mode-map
        ("j" . xref-next-line)
        ("k" . xref-prev-line)))

;; (use-package flycheck
;;   :if my/linux-p
;;   :ensure t
;;   :pin melpa
;;   :custom
;;   (flycheck-disabled-checkers '(python-ruff)))

(use-package highlight-indent-guides
  :ensure t
  :hook
  (python-mode . highlight-indent-guides-mode)
  (python-ts-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column))

(use-package docker
  :if my/linux-p
  :ensure t)

(use-package python
  :if my/linux-p
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :init
  (defun my/python-init-setup ()
    (setq tab-width python-indent-offset)
    (electric-operator-mode 1))
  :hook
  (python-ts-mode . my/python-init-setup)
  (python-mode . my/python-init-setup)
  :mode
  (("\\.py\\'" . python-ts-mode))
  :config
  (defvar-keymap python-indent-repeat-map
    :repeat t
    "<" #'python-indent-shift-left
    ">" #'python-indent-shift-right)
  (defun ruff-fix-buffer ()
    "Use ruff to fix lint violations in the current buffer."
    (interactive)
    (shell-command-to-string (format "ruff check --fix %s" (buffer-file-name)))
    (shell-command-to-string (format "ruff format %s" (buffer-file-name)))
    (revert-buffer t t t)))

;; (use-package lsp-mode
;;   :if my/linux-p
;;   :ensure t
;;   ;; :hook (lsp-after-open . my-reorder-eldoc-functions)
;;   :custom
;;   (lsp-diagnostics-provider :auto)
;;   (lsp-completion-provider :none)
;;   :init
;;   (with-eval-after-load 'tramp
;;     (add-to-list 'tramp-remote-path "/workspace/.venv/bin"))
;;   :hook
;;   (lsp-mode . (lambda () (when (file-remote-p default-directory)
;;                            (setq-local lsp-enable-file-watchers nil))))
;;   :config
;;   ;; lsp-booster
;;   (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;     "Try to parse bytecode instead of json."
;;     (or
;;      (when (equal (following-char) ?#)
;;        (let ((bytecode (read (current-buffer))))
;;          (when (byte-code-function-p bytecode)
;;            (funcall bytecode))))
;;      (apply old-fn args)))
;;   (advice-add (if (progn (require 'json)
;;                          (fboundp 'json-parse-buffer))
;;                   'json-parse-buffer
;;                 'json-read)
;;               :around
;;               #'lsp-booster--advice-json-parse)
;;   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;     "Prepend emacs-lsp-booster command to lsp CMD."
;;     (let ((orig-result (funcall old-fn cmd test?)))
;;       (if (and (not test?)                             ;; for check lsp-server-present?
;;                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;                lsp-use-plists
;;                (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;                (executable-find "emacs-lsp-booster"))
;;           (progn
;;             (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
;;               (setcar orig-result command-from-exec-path))
;;             (message "Using emacs-lsp-booster for %s!" orig-result)
;;             (cons "emacs-lsp-booster" orig-result))
;;         orig-result)))
;;   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;;   (dolist (re '("[/\\\\]\\.aws-sam\\'"
;;                 "[/\\\\]\\.cache\\'"
;;                 "[/\\\\]\\.claude\\'"
;;                 "[/\\\\]\\.devcontainer\\'"
;;                 "[/\\\\]\\.ruff_cache\\'"
;;                 "[/\\\\]\\.serena\\'"
;;                 "[/\\\\][^/\\\\]+\\.dist-info\\'"
;;                 "[/\\\\][^/\\\\]+\\.egg-info\\'"))
;;     (add-to-list 'lsp-file-watch-ignored-directories re)))

;; (use-package lsp-pyright
;;   :if my/linux-p
;;   :ensure t
;;   :hook
;;   ((python-mode python-ts-mode) . start-lsp-for-python)
;;   :init
;;   (defun start-lsp-for-python ()
;;     (require 'lsp-pyright)
;;     (lsp-deferred))
;;   :custom
;;   (lsp-pyright-langserver-command "basedpyright")
;;   ;; disable basedpyright specific features
;;   (lsp-pyright-basedpyright-inlay-hints-variable-types nil)
;;   (lsp-pyright-basedpyright-inlay-hints-call-argument-names nil)
;;   (lsp-pyright-basedpyright-inlay-hints-function-return-types nil)
;;   (lsp-pyright-basedpyright-inlay-hints-generic-types nil))

;; (use-package lsp-ruff
;;   :if my/linux-p
;;   :custom
;;   (lsp-ruff-log-level "debug"))

;; (use-package lsp-java
;;   :if my/linux-p
;;   :ensure t
;;   :hook (java-ts-mode . lsp-deferred)
;;   :custom
;;   (lsp-java-java-path "/usr/lib/jvm/java-21-openjdk-amd64/bin/java")
;;   :config
;;   (setq lsp-java-configuration-runtimes
;;         `[(:name "JavaSE-1.8" :path "/usr/lib/jvm/java-1.8.0-amazon-corretto" :default t)
;;           (:name "JavaSE-21"  :path "/usr/lib/jvm/java-21-openjdk-amd64")]))

;; (use-package lsp-ui
;;   :if my/linux-p
;;   :ensure t
;;   :after lsp-mode
;;   :init
;;   (defun kle/lsp-ui-doc-dwim ()
;;     (interactive)
;;     (if (lsp-ui-doc--frame-visible-p)
;;         (lsp-ui-doc-hide)
;;       (lsp-ui-doc-show)))
;;   :bind
;;   (:map lsp-ui-mode-map
;;         ("C-l C-d" . kle/lsp-ui-doc-dwim))
;;   :custom
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-alignment 'window)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-doc-max-width 150)
;;   (lsp-ui-doc-max-height 30)
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   ;; sidelineの日本語対応
;;   ;; 幅2の文字を考慮しておらず表示が崩れるので、関連している関数を全部書き直す
;;   (defun lsp-ui-sideline--make-display-string (info symbol current)
;;     "Make final string to display in buffer.
;;      INFO is the information to display.
;;      SYMBOL is the symbol associated with the info.
;;      CURRENT is non-nil when the point is on the symbol."
;;     (let* ((face (if current 'lsp-ui-sideline-current-symbol 'lsp-ui-sideline-symbol))
;;            (str (if lsp-ui-sideline-show-symbol
;;                     (concat info " " (propertize (concat " " symbol " ") 'face face))
;;                   info))
;;            (ch-len (length str))         ;; 文字数はプロパティ付与のために保持
;;            (vis-len (string-width str))  ;; 表示幅は string-width
;;            (margin (lsp-ui-sideline--margin-width)))
;;       (add-face-text-property 0 ch-len 'lsp-ui-sideline-global nil str)
;;       (concat
;;        (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align vis-len margin))))
;;        (propertize str 'display (lsp-ui-sideline--compute-height)))))

;;   ;; markdown-mode 側の special 変数の前方宣言。
;;   ;; 下の let で一時的に動的束縛しているが、markdown-mode 未ロード状態で
;;   ;; lexical-binding 下でバイトコンパイルされるとただのレキシカル変数になり
;;   ;; 効果が消えるので、同じ :config ブロック内で defvar しておく。
;;   (defvar markdown-hr-display-char)

;;   ;; push-info: final-string の長さ（表示幅）を string-width で計算する
;;   (defun lsp-ui-sideline--push-info (win-width symbol bounds info bol eol)
;;     (let* ((markdown-hr-display-char nil)
;;            (info (or (alist-get info lsp-ui-sideline--cached-infos)
;;                      (-some--> (lsp:hover-contents info)
;;                        (lsp-ui-sideline--extract-info it)
;;                        (lsp-ui-sideline--format-info it win-width)
;;                        (progn (push (cons info it) lsp-ui-sideline--cached-infos) it))))
;;            (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
;;       (when (and info
;;                  (> (string-width info) 0)
;;                  (lsp-ui-sideline--check-duplicate symbol info))
;;         (let* ((visible (if lsp-ui-sideline-show-symbol
;;                             (concat info " " (concat " " symbol " "))
;;                           info))
;;                (vis-w (string-width visible))
;;                (final-string (lsp-ui-sideline--make-display-string info symbol current))
;;                (pos-ov (lsp-ui-sideline--find-line vis-w bol eol))
;;                (ov (when pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
;;           (when pos-ov
;;             (overlay-put ov 'info info)
;;             (overlay-put ov 'symbol symbol)
;;             (overlay-put ov 'bounds bounds)
;;             (overlay-put ov 'current current)
;;             (overlay-put ov 'after-string final-string)
;;             (overlay-put ov 'before-string " ")
;;             (overlay-put ov 'window (get-buffer-window))
;;             (overlay-put ov 'kind 'info)
;;             (overlay-put ov 'position (car pos-ov))
;;             (push ov lsp-ui-sideline--ovs))))))

;;   ;; diagnostics: msg の幅を string-width で使う
;;   (defun lsp-ui-sideline--diagnostics (buffer bol eol)
;;     "Show diagnostics belonging to the current line."
;;     (when (and (bound-and-true-p flycheck-mode)
;;                (bound-and-true-p lsp-ui-sideline-mode)
;;                lsp-ui-sideline-show-diagnostics
;;                (eq (current-buffer) buffer))
;;       (lsp-ui-sideline--delete-kind 'diagnostics)
;;       (dolist (e (flycheck-overlay-errors-in bol (1+ eol)))
;;         (let* ((lines (--> (flycheck-error-format-message-and-id e)
;;                            (split-string it "\n")
;;                            (lsp-ui-sideline--split-long-lines it)))
;;                (display-lines (butlast lines (- (length lines) lsp-ui-sideline-diagnostic-max-lines)))
;;                (offset 1))
;;           (dolist (line (nreverse display-lines))
;;             (let* ((msg (string-trim (replace-regexp-in-string "[\t ]+" " " line)))
;;                    (msg (replace-regexp-in-string " " " " msg))
;;                    (ch-len (length msg))
;;                    (w-len (string-width msg))
;;                    (level (flycheck-error-level e))
;;                    (face (if (eq level 'info) 'success level))
;;                    (margin (lsp-ui-sideline--margin-width))
;;                    (msg (progn (add-face-text-property 0 ch-len 'lsp-ui-sideline-global nil msg)
;;                                (add-face-text-property 0 ch-len face nil msg)
;;                                msg))
;;                    (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align w-len margin))))
;;                                    (propertize msg 'display (lsp-ui-sideline--compute-height))))
;;                    (pos-ov (lsp-ui-sideline--find-line w-len bol eol t offset))
;;                    (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
;;               (when pos-ov
;;                 (setq offset (1+ (car (cdr pos-ov))))
;;                 (overlay-put ov 'after-string string)
;;                 (overlay-put ov 'kind 'diagnostics)
;;                 (overlay-put ov 'before-string " ")
;;                 (overlay-put ov 'position (car pos-ov))
;;                 (push ov lsp-ui-sideline--ovs))))))))

;;   ;; code-actions: タイトル幅を string-width で計算、画像は幅1とみなす
;;   (defun lsp-ui-sideline--code-actions (actions bol eol)
;;     "Show code ACTIONS."
;;     (let ((inhibit-modification-hooks t))
;;       (when lsp-ui-sideline-actions-kind-regex
;;         (setq actions (seq-filter (-lambda ((&CodeAction :kind?))
;;                                     (or (not kind?)
;;                                         (s-match lsp-ui-sideline-actions-kind-regex kind?)))
;;                                   actions)))
;;       (setq lsp-ui-sideline--code-actions actions)
;;       (lsp-ui-sideline--delete-kind 'actions)
;;       (seq-doseq (action actions)
;;         (-let* ((title (->> (lsp:code-action-title action)
;;                             (replace-regexp-in-string "[\n\t ]+" " ")
;;                             (replace-regexp-in-string " " " ")
;;                             (concat (unless lsp-ui-sideline-actions-icon
;;                                       lsp-ui-sideline-code-actions-prefix))))
;;                 (image (lsp-ui-sideline--code-actions-image action))
;;                 (margin (lsp-ui-sideline--margin-width))
;;                 (keymap (let ((map (make-sparse-keymap)))
;;                           (define-key map [down-mouse-1] (lambda () (interactive)
;;                                                            (save-excursion
;;                                                              (lsp-execute-code-action action))))
;;                           map))
;;                 (ch-len (length title))
;;                 (w-len (string-width title))
;;                 (img-w (if image 1 0))
;;                 (title (progn (add-face-text-property 0 ch-len 'lsp-ui-sideline-global nil title)
;;                               (add-face-text-property 0 ch-len 'lsp-ui-sideline-code-action nil title)
;;                               (add-text-properties 0 ch-len `(keymap ,keymap mouse-face highlight) title)
;;                               title))
;;                 (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align (+ w-len img-w) margin))))
;;                                 image
;;                                 (propertize title 'display (lsp-ui-sideline--compute-height))))
;;                 (pos-ov (lsp-ui-sideline--find-line (+ 1 w-len img-w) bol eol t))
;;                 (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
;;           (when pos-ov
;;             (overlay-put ov 'after-string string)
;;             (overlay-put ov 'before-string " ")
;;             (overlay-put ov 'kind 'actions)
;;             (overlay-put ov 'position (car pos-ov))
;;             (push ov lsp-ui-sideline--ovs)))))
;;     )
;;   )



;; (use-package dap-mode
;;   :if my/linux-p
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (require 'dap-python)
;;   (dap-auto-configure-mode 1)
;;   (setq dap-python-debugger 'debugpy))

;;; dired
(use-package lv :if my/linux-p :ensure t)
(use-package dired
  :custom
  ;; fix keybind for SKK
  (dired-bind-jump nil)
  (dired-kill-when-opening-new-dired-buffer t)
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'dired-mode 'emacs))
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
  ;; 辞書は locate-user-emacs-file 基準で解決し、存在するものだけ使う
  ;; (Windowsなど辞書未配置の環境でもエラーにならないように)
  (skk-large-jisyo (let ((jisyo (locate-user-emacs-file "skk-get-jisyo/SKK-JISYO.L")))
                     (and (file-exists-p jisyo) jisyo)))
  (skk-extra-jisyo-file-list
   (seq-filter #'file-exists-p
               (mapcar (lambda (name)
                         (locate-user-emacs-file (concat "skk-get-jisyo/" name)))
                       '("SKK-JISYO.jinmei"
                         "SKK-JISYO.fullname"
                         "SKK-JISYO.geo"
                         "SKK-JISYO.propernoun"
                         "SKK-JISYO.station"
                         "SKK-JISYO.law"
                         "SKK-JISYO.okinawa"))))
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

(use-package imenu-list
  :if my/linux-p
  :ensure t
  :custom
  (imenu-list-position 'left)
  (imenu-list-size 0.18)
  :bind
  ("C-;" . imenu-list-smart-toggle)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "j") 'next-line)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "k") 'previous-line)
    (evil-define-key 'normal imenu-list-major-mode-map (kbd "RET") 'imenu-list-goto-entry)))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-l m s" . magit-status)
         ("C-l m l c" . magit-log-current)
         ("C-l m l b" . magit-log-buffer-file))
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :init
  (with-eval-after-load 'shackle
    (add-to-list 'shackle-rules '(magit-status-mode :other right :size 0.4)))
  :config
  (defun suppress-iconify (&rest arg)
    (remove-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  (defun apply-iconify (&rest arg)
    (add-hook 'server-done-hook #'iconify-emacs-when-server-is-done))
  (advice-add 'magit-run-git-with-editor :before #'suppress-iconify)
  (advice-add 'with-editor-finish :after #'apply-iconify))

(use-package forge
  :ensure t
  :pin melpa-stable
  :after magit
  :config
  (setopt auth-sources '("~/.authinfo")))

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
  :hook (after-init . backward-forward-mode)
  :config
  (setq backward-forward-evil-compatibility-mode t)
  (with-eval-after-load 'evil
    (advice-add 'evil-goto-first-line :before #'backward-forward-push-mark-wrapper)
    (advice-add 'evil-goto-line :before #'backward-forward-push-mark-wrapper))
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
  (emacs-lisp-mode . (lambda () (smartparens-mode -1)))
  :config
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(use-package sudo-edit
  :if my/linux-p
  :ensure t)

(use-package google-translate
  :if my/linux-p
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
  :if my/linux-p
  :ensure t
  :bind
  (("M-%" . vr/query-replace)))

(use-package google-this
  :if my/linux-p
  :ensure t
  :bind
  (("C-l g" . google-this)))

(use-package yasnippet
  :if my/linux-p
  :ensure t
  :pin melpa
  :commands (yas-expand)
  :hook
  (prog-mode . yas-minor-mode)
  :bind (("C-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :if my/linux-p
  :ensure t
  :pin melpa
  :after yasnippet)

(use-package web-mode
  :if my/linux-p
  :ensure t
  :mode (;; ("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ;; ("\\.js\\'" . web-mode)
         ))

;; (use-package typescript-mode
;;   :if my/linux-p
;;   :ensure t)

(use-package tuareg
  :if my/linux-p
  :ensure t
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)
         ("\\.mly\\'" . tuareg-mode)
         ("\\.mll\\'" . tuareg-mode)
         ("\\.mlp\\'" . tuareg-mode)))

(use-package powershell
  :ensure t
  :mode (("\\.ps1\\'" . powershell-mode)))

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
  :if my/linux-p
  :ensure t)

(use-package rust-ts-mode
  :if my/linux-p
  :mode (("\\.rs\\'" . rust-ts-mode))
  :hook
  ((rust-mode . smartparens-mode)
   (rust-mode . electric-operator-mode))
  :custom
  (rust-format-on-save t))

;; (use-package auctex
;;   :if my/linux-p
;;   :ensure t
;;   :mode (("\\.tex\\'" . TeX-tex-mode)
;;          ("\\.latex\\'" . TeX-tex-mode))
;;   :custom
;;   (TeX-auto-save t)
;;   (TeX-parse-self t)
;;   ;; use tectonic as tex engine
;;   (TeX-engine-alist '((tectonic                          ; engine symbol
;;                        "Tectonic"                        ; engine name
;;                        "tectonic -X compile -f plain %T" ; shell command for compiling plain TeX documents
;;                        "tectonic -X watch"               ; shell command for compiling LaTeX documents
;;                        nil                               ; shell command for compiling ConTeXt documents
;;                        )))
;;   (TeX-engine 'tectonic)
;;   (LaTeX-command-style '(("" "%(latex) %(extraopts)")))
;;   (TeX-check-TeX nil)
;;   :config
;;   (use-package tex
;;     :config
;;     (let ((tex-list (assoc "TeX" TeX-command-list))
;;           (latex-list (assoc "LaTeX" TeX-command-list)))
;;       (setf (cadr tex-list) "%(tex)"
;;             (cadr latex-list) "%l"))))

(use-package vimrc-mode
  :if my/linux-p
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :init
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
        ("M-<left>" . kle/yaml-indent-shift-left)))

(use-package dockerfile-mode
  :if my/linux-p
  :ensure t)

(use-package textile-mode
  :if my/linux-p
  :ensure t
  :mode "\\.textile\\'")

(use-package yaml-pro
  :if my/linux-p
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
  :custom
  ;; Today's Agenda の欄は、案件をまたいだタスク管理の段の my/dashboard-insert-today
  (dashboard-items '((recents . 5) (my/today . 0)))
  :config
  (add-to-list 'dashboard-item-generators '(my/today . my/dashboard-insert-today))
  (keymap-set dashboard-mode-map "a" #'my/dashboard-open-agenda)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map (kbd "a") 'my/dashboard-open-agenda)
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
  (gptel-model 'gpt-5.6-terra))

(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install)
  :config
  (setopt gptel-magit-commit-prompt
          "Generate a single Git commit message following Conventional Commits v1.0.0.

### CORE PRINCIPLE

A commit message describes ONE primary purpose: the most valuable change in the diff.
When multiple changes are mixed in one diff:
- Choose the single most important change as the subject line.
- Priority for \"most important\": feat > fix > perf > refactor > others,
  but always judged by user-facing value, not by line count.
- Move the remaining meaningful changes into the body as bullet points.
- Never let cosmetic diffs (formatting, renames, generated code) become the subject.

### INPUT

You will receive the output of `git diff --cached` (unified diff format).
Capture WHAT changed and WHY at the level of intent, not a line-by-line restatement.
For a large diff, first identify the overarching intent, then the supporting changes.

### PROCESS

Follow these steps to decide the commit message structure:
1. Split the diff into groups by feature/purpose.
2. Decide a Conventional Commits type for each group.
3. Pick the single most important group -> it becomes the subject.
4. Put the other meaningful groups into the body as bullets.
5. Drop cosmetic-only diffs (formatting/rename/generated) from the subject.
The body is where you show the result of this analysis.
If more than one meaningful change exists, a body is REQUIRED.

### FORMAT

<type>[optional scope]: <description>

[optional body]

[optional footer(s)]

### TYPES

- feat: A new feature
- fix: A bug fix
- build: Changes to the build system or external dependencies
- ci: Changes to CI configuration files and scripts
- docs: Documentation only changes
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- style: Changes that do not affect the meaning of the code (formatting, etc.)
- test: Adding missing tests or correcting existing tests

### SCOPE

- Use a noun for the affected area (e.g., parser, api, auth, cli).
- Infer the scope from the file paths in the diff.
- Add a scope when the change is confined to a single module/component.
- Omit the scope when the change spans multiple areas or the whole project.

### BREAKING CHANGES

Indicate with `!` after the type/scope (e.g., `feat!:` or `feat(api)!:`)
or with a `BREAKING CHANGE:` footer.

### BODY

- Omit the body only when a single change is fully conveyed by the subject.
- When multiple changes exist, the body is REQUIRED.
- Use a bullet list (`- ` prefix), one meaningful change per bullet.
- Focus on WHY and what to watch out for, not a restatement of WHAT.

### AVOID

- Do not mechanically list file names, function names, or module names.
- Do not use empty verbs: \"〜を更新\", \"〜を変更\", \"各種修正\", \"〜関連の対応\".
- Do not make a large-but-cosmetic diff (formatting/rename) the subject.
- Do not stuff multiple purposes into one subject line.

### OUTPUT RULES

- Return ONLY the commit message text. No code fences, no commentary.
- Subject line: imperative mood, concise, no trailing period.

### LANGUAGE

- Write the commit message in Japanese.
- End the subject with a verb in dictionary form (e.g., 追加, 修正, 分離). Never use polite form (〜しました) or past tense (〜した).
- Keep type, scope, and BREAKING CHANGE keywords in English as-is.

### EXAMPLES

feat(auth): OAuth2によるログイン機能を追加

---

fix: ページネーションで最終ページが表示されない問題を修正

---

refactor(api): レスポンス生成処理をハンドラから分離

---

feat(parser)!: 設定ファイルのフォーマットをTOMLに変更

BREAKING CHANGE: YAML形式の設定ファイルはサポート外になります

---

The following example shows a commit with multiple changes.
The primary feature is the subject; supporting changes go in the body.

feat(editor): エージェント用ターミナルにタブ管理を追加

- tab-lineで複数セッションのタブ切り替えを可能にした
- 各タブにビジー状態のインジケータを表示
- 初期化処理を整理し関連設定を集約

---

BAD: feat: init.elを更新しtab-lineとhideshowとagent-shell-modeを変更
(This mechanically lists names without identifying the primary purpose.)

GOOD:
feat(editor): hideshowを有効化し全体トグルを追加

- agent-shellのtab-line表示とビジー表示も併せて整理
"
          )
  (setopt gptel-magit-diff-explain-prompt
          (concat gptel-magit-diff-explain-prompt
                  " Use Japanase for the answer."))
  ;; --- gptel-magit のローカルパッチ ---------------------------------------
  ;; 本体は2025-05以降更新が止まっており、以下の問題が残っている。
  ;;   1. LLM応答の整形(fill-region)が日本語のコミットメッセージを壊す
  ;;   2. コールバックが文字列以外(nil / (reasoning . TEXT))で呼ばれる想定がなく、
  ;;      nilだとmagitがnil引数を除去して `git commit --message --edit` が走り、
  ;;      "--edit" というメッセージで無確認コミットされる
  ;;   3. 常に `git diff --cached` を見るため、リワード時にdiffが空になる
  ;;   4. 生成結果を既存メッセージの先頭に挿入するため、リワード時に新旧が混ざる

  ;; 1. 整形を無効化する
  (defun my-gptel-magit--format-commit-message (message)
    message)
  (advice-add 'gptel-magit--format-commit-message :override
              #'my-gptel-magit--format-commit-message)

  ;; 2+3. diffの取得元を状況に応じて切り替え、応答は文字列のみ通す
  (defun my-gptel-magit--diff ()
    "Return the diff to describe.
Use the staged diff normally, and the diff of HEAD itself while rewording."
    (let ((staged (magit-git-output "diff" "--cached")))
      (if (string-blank-p staged)
          (magit-git-output "show" "--format=" "HEAD")
        staged)))

  (defun my-gptel-magit--generate (callback)
    "Generate a commit message and invoke CALLBACK with it."
    (gptel-magit--request (my-gptel-magit--diff)
      :system gptel-magit-commit-prompt
      :context nil
      :callback
      (lambda (response _info)
        (cond
         ((and (stringp response) (not (string-blank-p response)))
          (funcall callback (gptel-magit--format-commit-message response)))
         ;; 推論ブロックは本文より先に届くので黙って捨てる
         ((and (consp response) (eq (car response) 'reasoning)))
         (t (message "gptel-magit: コミットメッセージの生成に失敗しました"))))))
  (advice-add 'gptel-magit--generate :override #'my-gptel-magit--generate)

  ;; 4. 既存メッセージ(コメント行より前)を消してから挿入する
  (defun my-gptel-magit-generate-message ()
    "Generate a commit message, replacing the existing one."
    (interactive)
    (unless (magit-commit-message-buffer)
      (user-error "No commit in progress"))
    (gptel-magit--generate
     (lambda (message)
       (with-current-buffer (magit-commit-message-buffer)
         (save-excursion
           (goto-char (point-min))
           (delete-region
            (point-min)
            (if (re-search-forward (concat "^" comment-start) nil t)
                (max (point-min) (- (point) 2))
              (point-max)))
           (goto-char (point-min))
           (insert message)))))
    (message "gptel-magit: コミットメッセージを生成中..."))
  (advice-add 'gptel-magit-generate-message :override
              #'my-gptel-magit-generate-message)

  ;; 推論モデルでは reasoning.effort=none を指定し、推論ブロック自体を作らせない。
  ;; コミットメッセージ生成に推論は不要で、遅延と料金の無駄。
  ;; gpt-4.1 等の非推論モデルはこのパラメータで400になるため gpt-5 系限定。
  (defun my-gptel-magit--no-reasoning (orig &rest args)
    (let ((gptel--request-params
           (if (string-prefix-p "gpt-5" (format "%s" (or gptel-magit-model gptel-model)))
               (plist-put (copy-sequence gptel--request-params)
                          :reasoning '(:effort "none"))
             gptel--request-params)))
      (apply orig args)))
  (advice-add 'gptel-magit--request :around
              #'my-gptel-magit--no-reasoning)

  (setq gptel-magit-model 'gpt-5.6-luna))

(use-package emojify
  :ensure t
  :hook
  (after-init . global-emojify-mode)
  :custom
  (emojify-emoji-styles '(unicode github)))

;;; Linux specific setup
(when (eq system-type 'gnu/linux)
  (use-package ghostel
    :ensure t
    :init
    (setq ghostel-shell '("env" "-u" "TMUX" "tmux" "new-session" "-A" "-s" "main"))
    (defun toggle-ghostel (arg)
      "Toggle ghostel terminal in bottom window.  With prefix ARG, open fullscreen."
      (interactive "P")
      (if arg
          (ghostel)
        (if-let* ((buf (seq-find (lambda (b) (eq (buffer-local-value 'major-mode b) 'ghostel-mode))
                                (buffer-list)))
                 (win (get-buffer-window buf)))
            (delete-window win)
          (let ((display-buffer-overriding-action
                 '((display-buffer-in-direction) (direction . below) (window-height . 0.3))))
            (ghostel)))))
    (bind-key "<f10>" #'toggle-ghostel)
    (with-eval-after-load 'shackle
      (add-to-list 'shackle-rules '(ghostel-mode :align below :size 0.3)))
    :config
    (with-eval-after-load 'evil
      (evil-set-initial-state 'ghostel-mode 'emacs)
      (advice-add 'ghostel-copy-mode :after
                  (lambda (&rest _) (evil-normal-state)))
      (advice-add 'ghostel-semi-char-mode :after
                  (lambda (&rest _) (evil-emacs-state))))
    (bind-key "C-t" #'other-window-or-split ghostel-semi-char-mode-map))

  (use-package treesit-fold
    :init
    (let* ((elpa-lisp-dir "~/.emacs.d/elpa")
           (treesit-fold-file (concat elpa-lisp-dir "/treesit-fold/treesit-fold.el")))
      (unless (file-exists-p treesit-fold-file)
        (package-vc-install "https://github.com/emacs-tree-sitter/treesit-fold")))
    :bind
    ("C-l o" . treesit-fold-toggle))

  (use-package autodisass-java-bytecode
    :ensure t))
