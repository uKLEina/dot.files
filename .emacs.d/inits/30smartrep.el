(use-package smartrep
  :config
  (smartrep-define-key
      global-map "C-l"
    '(("<tab>" . tab-to-tab-stop)))
  (smartrep-define-key
      global-map "C-x"
    '(("}" . enlarge-window-horizontally)
      ("{" . shrink-window-horizontally))))
