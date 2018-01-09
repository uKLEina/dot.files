(use-package org
  :mode (("\\.org\\'" . org-mode))
  :init
  ;; reftex with org mode
  (add-hook 'org-mode-hook 'turn-on-reftex)
  :config
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c [") 'reftex-citation))
  ;; Org Mode LaTeX Export
  (use-package ox-latex)
  (use-package ox-beamer)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-latex-default-class "jsarticle")
  ;; org-latex-classes
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[12pt,a4j,uplatex]{jsarticle}
                 [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 ))
  (custom-set-variables '(org-latex-classes (delete (assoc "beamer" org-latex-classes) org-latex-classes)))
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[dvipdfmx,presentation]{beamer}
                 [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  )
