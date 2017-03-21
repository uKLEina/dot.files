(setq set-mark-command-repeat-pop t);カーソルを移動したあとC-u C-SPC(連打)で戻れる

;;; language environment
(set-language-environment "Japanese")
(auto-compression-mode t)
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

;;; util
(savehist-mode 1)
(setq-default save-place t)
(require 'saveplace)
(show-paren-mode 1)
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
(setq scroll-step 1)
;; when dabbrev distinguish capital and small letter
(require 'dabbrev)
(setq dabbrev-case-fold-search nil)
(setq initial-scratch-message "")
(setq delete-auto-save-files t)
;; show filename and path in title bar
(setq frame-title-format "%b (%f)")
;; hide tool bar/scroll bar/menu bar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq require-final-newline t)

;; ファイル名問い合わせで大文字小文字の区別をしない
;;;
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)
(global-auto-revert-mode 1)

;; バッファの開始・終端を明示する
(setq-default indicate-buffer-boundaries 'left)

;;; visible spaces
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "gray"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-u-1 prepend)
     ("[ ]+$" 0 my-face-b-2 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks
          '(lambda ()
             (if font-lock-mode nil
               (font-lock-mode t))
             (font-lock-fontify-buffer)) t)


(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; server関連
(require 'server)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
(add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
;(defalias 'exit 'save-buffers-kill-emacs)

;; open URL in the browser
(ffap-bindings)

;; buffername in same filename
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers "*[^*]+*")

;; バックアップファイルはうっとおしいので一箇所にまとめてしまう
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup" t)))

;; move to function difinition
;; C-x F -> 関数定義へ移動
;; C-x K -> キーにバインドされている関数定義へ移動
;; C-x V -> 変数定義へ移動
(find-function-setup-keys)

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

(defun my-count-string-columns (str)
  "Count columns of string. The number of column begins 0."
  (with-temp-buffer
    (insert str)
    (current-column)))

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

;;; check ip
;; (defun machine-ip-address (dev)
;;   "Return IP address of a network device."
;;   (let ((info (network-interface-info dev)))
;;  (if info
;;    (format-network-address (car info) t))))

;; (defvar *network-interface-names* '("en1" "wlan0" "eth6")
;;   "Candidates for the network devices.")

;; (defun labp ()
;;   "If I'm in the lab, my IP address is:'192.168.(10|11).*'."
;;   (let ((ip (some #'machine-ip-address *network-interface-names*)))
;;   (and ip
;;        (eq 0 (or (string-match "^192\\.168\\.10\\." ip)
;;                  (string-match "^192\\.168\\.11\\." ip))))))

;;; window
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-t") 'other-window-or-split)

(if window-system (add-to-list 'default-frame-alist '(alpha . 90)))

;;; eww
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;;; hippie expand source
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
