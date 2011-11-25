(setq erlang-root-dir "/usr/local/lib/erlang")

(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.6.5/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

(add-to-list 'ac-modes 'erlang-mode)

(add-hook 'erlang-mode-hook 'run-coding-hook)
(add-hook 'erlang-mode-hook 'turn-on-flymake)

(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name temp-file
		(file-name-directory buffer-file-name))))
    (list "~/.emacs.d/extra-libs/eflymake/eflymake" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

(require 'erlang-start)
