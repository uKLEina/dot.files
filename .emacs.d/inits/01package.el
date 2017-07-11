(use-package package
  :defer t
  :init
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("org" . "http://orgmode.org/elpa/")))
  (setq package-archive-priorities
        '(("melpa-stable" . 30)
          ("org" . 20)
          ("gnu" . 10)
          ("melpa" . 0))))
