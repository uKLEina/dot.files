(require 'smartrep)
(smartrep-define-key
    python-mode-map "C-c"
  '((">" . py-shift-right)
    ("<" . py-shift-left)))
