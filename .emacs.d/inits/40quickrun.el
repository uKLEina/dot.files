(use-package quickrun
  :bind (("<f5>" . quickrun))
  :config
  ;; ipython
  (custom-set-variables '(quickrun-timeout-seconds nil))
  (quickrun-add-command "ipython"
                        '((:command . "ipython")
                          (:exec . "%c %s"))
                        :mode 'python-mode)
  (push '("*quickrun*" :position bottom :width 30)
        popwin:special-display-config))
