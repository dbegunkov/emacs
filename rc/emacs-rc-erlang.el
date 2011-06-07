;;; emacs-rc-erlang.el ---

(setq erlang-root-dir "/usr/local/lib/erlang")

(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.6.4/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

(when (and (require 'erlang-start nil t)
           (require 'erlang-flymake nil t))
  (erlang-flymake-only-on-save))

(add-hook 'erlang-mode-hook 'run-coding-hook)

;;; emacs-rc-erlang.el ends here
