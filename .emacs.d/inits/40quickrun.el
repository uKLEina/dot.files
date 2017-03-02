(require 'quickrun)
(global-set-key (kbd "<f5>") 'quickrun)

;;; ipython
(quickrun-add-command "ipython"
  '((:command . "ipython")
    (:exec . "%c %s"))
  :mode 'python-mode)

(push '("*quickrun*" :position bottom :width 30)
      popwin:special-display-config)
