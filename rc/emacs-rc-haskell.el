(when (load "haskell-site-file.el" t nil nil)
  (add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))

  (add-hook 'haskell-mode-hook 'run-coding-hook)
  (add-hook 'haskell-mode-hook '(lambda ()
				  (turn-on-haskell-doc-mode)
				  (turn-on-haskell-indentation)
                  (setq haskell-font-lock-symbols t))))
