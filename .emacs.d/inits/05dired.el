(use-package dired
  :defer t
  :config
  ;; サイズ表示
  (setq dired-listing-switches "-alh")
  ;; sort
  (defvar dired-sort-order '("" "t" "S" "X")
    "-t (時間) -X (拡張子) -S (サイズ) なし (アルファベット順) を切り替える。")
  (defvar dired-sort-order-position 0)
  (defun dired-rotate-sort ()
    "Rotate dired toggle sorting order by `dired-sort-order'"
    (interactive)
    (setq dired-sort-order-position
          (% (1+ dired-sort-order-position) (length dired-sort-order)))
    (setq dired-actual-switches
          (concat dired-listing-switches (elt dired-sort-order
                                              dired-sort-order-position)))
    (dired-sort-other dired-actual-switches))
  (bind-keys :map dired-mode-map
             ("s" . dired-rotate-sort)
             ("r". wdired-change-to-wdired-mode)))
