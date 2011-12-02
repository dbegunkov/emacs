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
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-visualize)
(global-set-key (kbd "C-x C-z") nil) ;;I hate that suspend feature
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "C-.") 'textmate-shift-right)
(global-set-key (kbd "C-,") 'textmate-shift-left)

(setq cua-enable-cua-keys nil) ;; only for rectangles
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(cua-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-t") 'ace-jump-word-mode)
(define-key global-map (kbd "M-t") 'ace-jump-line-mode)

(require 'deft)
(global-set-key [f8] 'deft)

(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-to-char)
