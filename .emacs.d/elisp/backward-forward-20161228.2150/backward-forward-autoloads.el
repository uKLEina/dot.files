;;; backward-forward-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "backward-forward" "backward-forward.el" (22884
;;;;;;  8657 689499 563000))
;;; Generated autoloads from backward-forward.el

(defvar backward-forward-mode nil "\
Non-nil if Backward-Forward mode is enabled.
See the `backward-forward-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `backward-forward-mode'.")

(custom-autoload 'backward-forward-mode "backward-forward" nil)

(autoload 'backward-forward-mode "backward-forward" "\
enables or disable backward-forward minor mode.

when backward-forward mode is enabled, it keeps track of mark pushes across
all buffers in a variable backward-forward-mark-ring, and allows you to navigate backwards
and forwards across these marks using <C-left> and <C-right>.  to customize
the navigation behavior one must customize the mark pushing behavior --
add 'advice' to a command to make it push a mark before invocation if you
want it to be tracked.  see backward-forward.el for examples and more
information.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; backward-forward-autoloads.el ends here
