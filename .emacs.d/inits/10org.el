(use-package org
  :defer t
  :init
  ;; reftex with org mode
  (add-hook 'org-mode-hook 'turn-on-reftex)
  :config
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c [") 'reftex-citation)))

;; Org Mode LaTeX Export
(use-package ox-latex
  :defer t
  :config
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  ;; pdf process = latexmk
  (setq org-latex-pdf-process '("latexmk %f"))
  ;; default class = jsarticle
  (setq org-latex-default-class "jsarticle")
  ;; org-latex-classes
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[11pt,a4paper,uplatex]{jsarticle}
                [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 ))
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[dvipdfmx,presentation]{beamer}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  ;; org-export-latex-no-toc
  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)
  )
