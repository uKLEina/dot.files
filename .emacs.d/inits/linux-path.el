(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-copy-envs '("PATH" "MANPATH" "PYTHONPATH"))
