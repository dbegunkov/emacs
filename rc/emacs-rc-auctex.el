(require 'tex-site)
(require 'preview-latex)

(setq
 TeX-auto-save t
 TeX-parse-self t
 TeX-DVI-via-PDFTeX t)



(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (TeX-fold-mode 1)
             (TeX-PDF-mode 1)
             (outline-minor-mode 1)
             (add-to-list 'TeX-command-list
                          '("XeLaTeX" "xelatex -interaction=nonstopmode -shell-escape %s"
                            TeX-run-command t t :help "Run xelatex") t)))
