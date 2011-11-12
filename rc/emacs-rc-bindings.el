;; Bells and whistles
(defcustom solarized-cycle-next 'solarized-dark ""
  :group 'solarized)
(defcustom solarized-cycle-current 'solarized-light ""
  :group 'solarized)
(defun cycle-solarized ()
  "Cycles through solarized-dark and solarized-light"
  (interactive)
  (let ((next solarized-cycle-next)
        (curr solarized-cycle-current))
    (setq solarized-cycle-next curr)
    (setq solarized-cycle-current next)
    (load-theme next)
    (message "%s" next)))

(global-set-key (kbd "C-M-`") 'cycle-solarized)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)

;; File finding
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(when (fboundp 'recentf-mode)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

;; Editing
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)
