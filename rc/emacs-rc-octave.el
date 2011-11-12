(add-to-list 'auto-mode-alist  '("\\.m$" . octave-mode))

(add-hook 'octave-mode-hook 'run-coding-hook)
