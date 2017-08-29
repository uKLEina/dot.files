(use-package plantuml-mode
  :defer t
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode)
         ("\\.pu\\'" . plantuml-mode))
  :config
  (custom-set-variables '(plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))
  (add-hook 'plantuml-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (plantuml-preview-buffer 4)) nil 'make-it-local)))
  (push '("*PLANTUML Preview*" :position right :width 50 :noselect t)
        popwin:special-display-config)
  )
