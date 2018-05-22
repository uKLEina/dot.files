;; (use-package tex-jp
;;   :defer t
;;   :init
;;   (defun latex-mode-setup ()
;;     "hook function to setup `LaTeX-mode'"
;;     (add-to-list 'TeX-command-list
;;                  '("Latexmk"
;;                    "latexmk %t"
;;                    TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
;;     (add-to-list 'TeX-command-list
;;                  '("Latexmk-upLaTeX"
;;                    "latexmk -e '$latex=q/uplatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
;;                    TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX"))
;;     (add-to-list 'TeX-command-list
;;                  '("Latexmk-LuaLaTeX"
;;                    "latexmk -e '$lualatex=q/lualatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdflua %t"
;;                    TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
;;     (add-to-list 'TeX-command-list
;;                  '("Zathura"
;;                    "zathura -x \"emacsclient --no-wait +%%{line} %%{input}\" --synctex-forward \"%n:0:%b\" %s.pdf"
;;                    TeX-run-discard-or-function t t :help "Forward and inverse search with zathura"))
;;     )
;;   (add-hook 'LaTeX-mode-hook 'latex-mode-setup)
;;   (add-hook 'LaTeX-mode-hook 'japanese-latex-mode)
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;   :config
;;   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;   (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
;;     (delq (assoc command TeX-command-list) TeX-command-list))
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)
;;   (setq reftex-plug-into-AUCTeX t)
;;   (setq japanese-TeX-engine-default 'uptex)
;;   (setq TeX-engine 'uptex)
;;   (setq TeX-PDF-from-DVI "Dvipdfmx")
;;   (setq japanese-LaTeX-default-style "bxjsarticle")
;;   (setq TeX-view-program-selection '((output-pdf "Zathura")))
;;   (setq TeX-source-correlate-method 'synctex)
;;   (setq TeX-source-correlate-start-server t)
;;   (setq TeX-source-correlate-mode t)
;;   )


(with-eval-after-load 'tex-jp
  (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
    (delq (assoc command TeX-command-list) TeX-command-list)))
(setq japanese-TeX-engine-default 'uptex)
(setq japanese-LaTeX-default-style "bxjsarticle")
(setq TeX-engine 'uptex)
(setq TeX-PDF-from-DVI "Dvipdfmx")
(setq TeX-view-program-selection '((output-pdf "Evince")))
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-mode t)
(add-hook 'LaTeX-mode-hook 'japanese-latex-mode)
(with-eval-after-load 'tex-jp
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (add-to-list 'TeX-command-list
                                   '("Latexmk"
                                     "latexmk %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
                      (add-to-list 'TeX-command-list
                                   '("Latexmk-upLaTeX"
                                     "latexmk -e '$latex=q/uplatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX"))
                      (add-to-list 'TeX-command-list
                                   '("Latexmk-LuaLaTeX"
                                     "latexmk -e '$lualatex=q/lualatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdflua %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
                      (add-to-list 'TeX-command-list
                                   '("Zathura"
                                     "zathura -x \"emacsclient --no-wait +%%{line} %%{input}\" --synctex-forward \"%n:0:%b\" %s.pdf"
                                     TeX-run-discard-or-function t t :help "Forward and inverse search with zathura"))
                      )))

;;
;; RefTeX with AUCTeX
;;
(with-eval-after-load 'tex-jp
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
(setq reftex-plug-into-AUCTeX t)
