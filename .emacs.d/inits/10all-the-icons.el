(defun custom-modeline-modified ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken")
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link")
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock")))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (apply (cadr result) (cddr result))
                'face `(:family ,(funcall (car result))))))

(defun custom-modeline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer))
                          )))))

(defun custom-modeline-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((warn-count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                    (or .warning 0)))
                                      (error-count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                     (or .error 0))))
                                  (format "✖E%s/W%s" error-count warn-count))
                              "✔No Issues"))
                 (`running     "⟲Running")
                 (`no-checker  "⚠No Checker")
                 (`not-checked "✖Disabled")
                 (`errored     "⚠Error")
                 (`interrupted "⛔Interrupted")
                 (`suspicious  ""))))
    (format " %s"
            (propertize text
                        'help-echo "Show Flycheck Errors"
                        'mouse-face '(:box 1)
                        'local-map (make-mode-line-mouse-map
                                    'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))

(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git")))
     (propertize (format "<%s>" branch))
     )))

(defun -custom-modeline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.0) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
     ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
     (t (format "%s" vc-mode)))))

(defun custom-modeline-minor-mode ()
  (let ((minor-modes (format-mode-line minor-mode-alist)))
    (if (eq minor-modes "")
        ""
      (propertize (format " (%s)" (substring minor-modes 1))))))

(defun custom-modeline-linum-colnum ()
  (format " [L%s/%s C%s]" (format-mode-line "%l") (line-number-at-pos (point-max)) (format-mode-line "%c")))

(defun custom-modeline-buffer-name ()
  (let ((buffer-name (format-mode-line "%b")))
    (if (string= (substring buffer-name 0 1) "*")
        (format " %s" buffer-name)
      (format " %s%s" default-directory buffer-name))))

(defun remove-padding-zero (num)
  (if (string= (substring num 0 1) "0")
      (substring num 1)
    num))

(defun custom-modeline-datetime ()
  (let* ((system-time-locale "C")
         (dow (format "%s" (format-time-string "%a")))
         (month (format " %s" (format-time-string "%b")))
         (day (format " %s" (remove-padding-zero (format-time-string "%d"))))
         (hour (format " %s" (remove-padding-zero (format-time-string "%I"))))
         (minute (format-time-string "%M")))
    (propertize
     (concat
      hour
      ":"
      minute
      (format-time-string "%p")
      )
     'help-echo "Show calendar"
     'mouse-face '(:box 1)
     'local-map (make-mode-line-mouse-map
                 'mouse-1 (lambda () (interactive) (calendar))))
    ))

(defun custom-modeline-python-venv ()
  (if (string= major-mode "python-mode")
      (let ((venv-name (if (or (not (boundp 'venv-current-name))
                               (eq venv-current-name nil))
                           "GLOBAL"
                         venv-current-name)))
        (format " [venv: %s]" venv-name))
    ""))

(use-package all-the-icons
  :config
  (setq-default mode-line-format
        '((:eval
           (concat
            " "
            "%e"
            (custom-modeline-modified)
            "(%z)"
            " %IB"
            (custom-modeline-mode-icon)
            (custom-modeline-buffer-name)
            (custom-modeline-flycheck-status)
            (custom-modeline-icon-vc)
            (custom-modeline-python-venv)
            (custom-modeline-minor-mode)
            (custom-modeline-linum-colnum)
            (custom-modeline-datetime)
            )))))
