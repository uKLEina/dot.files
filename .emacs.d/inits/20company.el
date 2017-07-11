(use-package company
  :defer t
  :config
  (company-quickhelp-mode +1)
  (bind-keys :map company-active-map
             ;; C-n, C-pで次/前の補完候補を選択
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ;; C-sで絞り込む
             ("C-s" . company-filter-candidates)
             ;; TABで候補を設定
             ("C-i" . company-complete-selection))
  (bind-keys :map company-search-map
             ;; C-n, C-pで次/前の補完候補を選択
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))
