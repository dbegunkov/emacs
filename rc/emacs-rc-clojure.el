;; clojure-mode
(require 'clojure-mode)

;; paredit
;(require 'paredit)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
;	(defun paredit-mode-enable () (paredit-mode 1))
;	(add-hook 'slime-mode-hook 'paredit-mode-enable)
;	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
	(setq slime-protocol-version 'ignore)))

(require 'slime)
(slime-setup)

(add-hook 'clojure-mode-hook 'run-coding-hook)
