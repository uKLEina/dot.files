(use-package ddskk
  :defer t
  :bind (("C-x C-j" . skk-mode)
         ("C-x j" . skk-auto-fill-mode))
  :init
  (setq skk-user-directory "~/.skk.d")
  :config
  (setq skk-large-jisyo "~/.skk.d/dic/SKK-JISYO.L")

  ;; 補完候補のリスト表示
  (setq skk-dcomp-activate t)
  (setq skk-dcomp-multiple-activate t)
  ;; 入力語「；」で絞り込み
  (require skk-hint)
  ;; カタカナ語の通常変換および辞書登録
  (setq skk-search-katakana nil)
  ;; 対応括弧の自動入力
  (setq skk-auto-insert-paren t)
  ;; 変換の学習
  (require 'skk-study)
  ;; 追加の辞書
  (setq skk-extra-jisyo-file-list
        (list "~/.skk.d/dic/SKK-JISYO.geo"
              "~/.skk.d/dic/SKK-JISYO.jinmei"
              "~/.skk.d/dic/SKK-JISYO.propernoun"
              "~/.skk.d/dic/SKK-JISYO.station"))
  ;; 変換候補を別バッファに表示
  (setq skk-show-candidates-always-pop-to-buffer t)
  ;; アノテーションを表示(C-iでWikipediaの解説を呼べる)
  (setq skk-show-annotation t)
  ;; isearchでskkを利用
  (setq skk-isearch-mode-enable 'always))
