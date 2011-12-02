(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key (kbd "S-<ret>") 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key (kbd "C-<up>")
                            'comint-previous-matching-input-from-input)
             (local-set-key (kbd "C-<down>")
                            'comint-next-matching-input-from-input)))
(require 'ess-site)

(setq comint-input-ring-size 1000)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq ess-ask-for-ess-directory t)
(setq ess-ask-about-transfile nil)
(setq ess-eval-visibly-p t)
(ess-toggle-underscore nil)

(add-hook 'ess-mode-hook 'run-coding-hook)
(add-to-list 'ac-modes 'ess-mode)
