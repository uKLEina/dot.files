(require 'smartrep)
(require 'python-mode)

(smartrep-define-key
    python-mode-map "C-c"
  '((">" . py-shift-right)
    ("<" . py-shift-left)))

(smartrep-define-key
    global-map "C-l"
  '(("TAB" . tab-to-tab-stop)))

(smartrep-define-key
    flycheck-mode-map "C-c !"
  '(("n" . flycheck-next-error)
    ("p" . flycheck-previous-error)))
