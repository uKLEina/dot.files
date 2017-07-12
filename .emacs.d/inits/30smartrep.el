(use-package smartrep
  :commands (smartrep-define-key)
  :config
  (smartrep-define-key
      global-map "C-l"
    '(("<tab>" . tab-to-tab-stop))))
