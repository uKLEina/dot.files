;; keybind for built-in commands
;; all other user original bindings are defined in
;; other setting files/just after commands defined

;; 現在のメジャー・マイナーモードに関するHELPは <f1> m (describe-mode)
;; 特定のモードに関するHELPは <f1> f foobar (describe-funcrion)
;; 特定のコマンドに関するHELPは <f1> k (describe-mode)
;; 現時点で利用可能なキーバインドを表示 <f1> b

;; 正規表現を対話的に作成 M-x re-builder(helm-regexp)
;; バッファ内を正規表現で検索 M-x occure
;; iserach から occur に切り換え M-s o
;; 文字列置換 M-%
;; 正規表現置換 C-M-%

;; 引用マーク入れ 範囲選択して C-x r t もしくは (string-rectangle)

;; 桁揃え M-x align もしくは C-u M-x align

;; 2つのファイルの相違点を表示 (ediff-files) 3つなら (ediff-files3)
;; 2つのファイルをマージする (ediff-merge-files)

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-;") 'hippie-expand)
(global-set-key (kbd "C-x p") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "C-x r e") 'helm-regexp)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-S-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (scroll-down 1)))

;; 自分用prefixはC-lで統一
;; 元のC-lはC-l C-lに
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l C-l") 'recenter-top-bottom)
(global-set-key (kbd "C-l C-x") 'server-edit)
(global-set-key (kbd "C-l <C-tab>") 'tab-to-tab-stop)
(global-set-key (kbd "C-l C-n") 'find-name-dired)
