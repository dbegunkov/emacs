(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun turn-on-hideshow () (hs-minor-mode t))

(defun turn-on-linum () (linum-mode t))

(defun turn-on-undo-tree-mode () (undo-tree-mode t))

(defun turn-on-flymake () (flymake-mode t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\):"
      1 font-lock-warning-face t))))

(add-hook 'coding-hook 'electric-pair-mode)
(add-hook 'coding-hook 'electric-layout-mode)
(add-hook 'coding-hook 'electric-indent-mode)
(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'add-watchwords)
;(add-hook 'coding-hook 'turn-on-hideshow)
(add-hook 'coding-hook 'turn-on-linum)
(add-hook 'coding-hook 'auto-complete-mode)
(add-hook 'coding-hook 'rainbow-delimiters-mode)
(add-hook 'coding-hook 'turn-on-undo-tree-mode)

(defun run-coding-hook ()
  (interactive)
  (run-hooks 'coding-hook))

(defun move-line (arg)
  "Moves line up or down, depending on the arg."
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (if (eql arg 1) (forward-line))
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (move-line -1))

(defun move-line-down ()
  (interactive)
  (move-line 1))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


;; The following two function are taken from textmate.el package
;; by defunkt.
(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.

A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))


(defun text-scale-normal-size ()
  (interactive)
  (text-scale-increase 0))


(defun make-password (&optional length)
  (let* ((length (or length 8))
         (alphabet "12345!@#$%%qwertQWERTasdfgASDFGzxcvbZXCVB")
         (command (format "echo `< /dev/urandom tr -dc '%s' | head -c%i`"
                          alphabet length)))
    (shell-command-to-string command)))

(eval-after-load "flymake"
  '(progn
    (defun flymake-after-change-function (start stop len)
      "Start syntax check for current buffer if it isn't already running."
      ;; Do nothing, don't want to run checks until I save.
      )))
