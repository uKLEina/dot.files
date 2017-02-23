(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; suppress the warning buffer pup-up
;; M-x auto-compile-display-log to manually open it
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)
