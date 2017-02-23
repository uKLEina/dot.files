(defun region-to-single-quote ()
  (interactive)
  (quote-formater "'%s'" "^\\(\"\\).*" ".*\\(\"\\)$"))

(defun region-to-double-quote ()
  (interactive)
  (quote-formater "\"%s\"" "^\\('\\).*" ".*\\('\\)$"))

(defun region-to-bracket ()
  (interactive)
  (quote-formater "\(%s\)" "^\\(\\[\\).*" ".*\\(\\]\\)$"))

(defun region-to-square-bracket ()
  (interactive)
  (quote-formater "\[%s\]" "^\\(\(\\).*" ".*\\(\)\\)$"))

(defun quote-formater (quote-format re-prefix re-suffix)
  (if mark-active
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (replace-func (lambda (re target-text)(replace-regexp-in-string re "" target-text nil nil 1)))
             (text (funcall replace-func re-suffix (funcall replace-func re-prefix region-text))))
        (delete-region (region-beginning) (region-end))
        (insert (format quote-format text)))
    (error "Not Region selection")))


(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map (kbd "M-'") 'region-to-single-quote)
(define-key region-bindings-mode-map (kbd "M-\"") 'region-to-double-quote)
(define-key region-bindings-mode-map (kbd "M-8") 'region-to-bracket)
(define-key region-bindings-mode-map (kbd "M-[") 'region-to-square-bracket)

(provide 'region-wrap)
