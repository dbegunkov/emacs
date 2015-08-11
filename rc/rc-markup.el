;;; rc-markup.el ---


;; Markdown

(use-package markdown-mode
  :ensure markdown-mode
  :commands markdown-mode
  :mode "\\.md\\|\\.markdown")

;; YAML

(use-package yaml-mode
  :ensure yaml-mode)

;; HTML, CSS

(add-hook 'css-mode-hook
          '(lambda()
             (setq css-indent-offset 2)))

;; (add-hook 'sgml-mode-hook (lambda () (setq tab-width 2)))

(use-package rainbow-mode
  :ensure rainbow-mode
  :diminish rainbow-mode
  :config (progn
            (add-hook 'html-mode-hook 'rainbow-turn-on)
            (add-hook 'css-mode-hook 'rainbow-turn-on)))

(use-package smartparens
  :ensure smartparens
  :config (sp-with-modes '(html-mode sgml-mode)
            (sp-local-pair "<" ">")))

(use-package web-mode
  :ensure web-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
            (add-hook 'web-mode-hook '(lambda() (whitespace-mode -1)))
            (setq web-mode-engines-alist
                  '(("django"    . "\\.html\\'")))
            (setq web-mode-markup-indent-offset 4
                  web-mode-indent-style 4
                  web-mode-code-indent-offset 4)
            (setq web-mode-enable-auto-pairing nil)
            (setq whitespace-global-modes '(not web-mode))))

;; gettext

;; (when (require 'po-mode nil t)
;;   (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
;;   (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t))

;; LaTeX via AucTeX

(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
           '("%`%l%(mode) -shell-escape%' %t"
             TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

;; (add-hook 'LaTeX-mode-hook '(lambda ()
;;                               (TeX-PDF-mode 1)))

(use-package company-auctex
  :ensure company-auctex)

(use-package tex-site
  :ensure auctex
  :config (progn
          (when-osx
              (setq TeX-view-program-list '(("Preview" "open %o"))
                    TeX-view-program-selection '((output-pdf "Preview"))))

          (require 'texmathp)

          (setq TeX-auto-save t
                TeX-parse-self t
                TeX-DVI-via-PDFTeX t)

          (company-auctex-init)

          (add-hook 'LaTeX-mode-hook '(lambda ()
                                        ;; (LaTeX-math-mode 1)
                                        (TeX-fold-mode 1)
                                        (TeX-PDF-mode 1)
                                        (outline-minor-mode 1)))))


;;; rc-markup.el ends here
