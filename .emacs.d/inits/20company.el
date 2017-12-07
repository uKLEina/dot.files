(use-package company
  :defer t
  :commands (company-mode-on)
  :config
  (custom-set-variables '(company-idle-delay 0)
                        '(company-minimum-prefix-length 2)
                        '(company-selection-wrap-around t))
  (company-quickhelp-mode +1)
  (bind-key "C-M-i" 'company-complete company-mode-map)
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
