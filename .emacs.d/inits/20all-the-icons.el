(defun custom-modeline-modified ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.0 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.0 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.0 :v-adjust 0.1)))
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
                          'display '(raise 0.01)
                          )))))

(defun custom-modeline-flycheck-status ()
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                               (+ (or .warning 0) (or .error 0)))))
                                  (format "✖%s Issue%s" count (unless (eq 1 count) "s")))
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
     ;; (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.0) 'display '(raise -0.1))
     ;; " · "
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " <%s>" branch) 'face `(:height 1.0)))))

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

(use-package all-the-icons
  :config
  (setq mode-line-format
        '("%e"
          (:eval
           (concat
            (custom-modeline-modified)
            (custom-modeline-mode-icon)
            " "
            default-directory
            (buffer-name)
            "           (%m)"
            (custom-modeline-flycheck-status)
            (custom-modeline-icon-vc)
            )))))
