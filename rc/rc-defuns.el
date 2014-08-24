;;; rc-misc.el ---

;; The following two functions are from
;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


;; The next three functions are taken from the awesome 'Emacs Prelude'
;; project, already mentioned elsewhere.
(defun delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))


(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

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

;; taken from Emacs Live

(defun live-bounds-of-preceding-sexp ()
  "Return the bounds of sexp before the point. Copies semantics
   directly from the fn preceding-sexp to ensure highlighted area
   is identical to that which is evaluated."
  (let ((opoint (point))
        ignore-quotes
        expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
        ;; If this sexp appears to be enclosed in `...'
        ;; then ignore the surrounding quotes.
        (setq ignore-quotes
              (or (eq (following-char) ?\')
                  (eq (preceding-char) ?\')))
        (forward-sexp -1)
        ;; If we were after `?\e' (or similar case),
        ;; use the whole thing, not just the `e'.
        (when (eq (preceding-char) ?\\)
          (forward-char -1)
          (when (eq (preceding-char) ??)
            (forward-char -1)))

        ;; Skip over hash table read syntax.
        (and (> (point) (1+ (point-min)))
             (looking-back "#s" (- (point) 2))
             (forward-char -2))

        ;; Skip over `#N='s.
        (when (eq (preceding-char) ?=)
          (let (labeled-p)
            (save-excursion
              (skip-chars-backward "0-9#=")
              (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
            (when labeled-p
              (forward-sexp -1))))

        (save-restriction
          ;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
          ;; `variable' so that the value is returned, not the
          ;; name
          (if (and ignore-quotes
                   (eq (following-char) ?`))
              (forward-char))
          (cons (point) opoint))))))

(defun live-bounds-of-defun ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn eval-defun-2 to ensure highlighted area
   is identical to that which is evaluated."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (setq beg (point))
    (read (current-buffer))
    (setq end (point))
    (cons beg end)))

;; fix up esf to highlight exactly what emacs evaluates
(defun live-esf-initialize-elisp ()
  (define-eval-sexp-fu-flash-command eval-last-sexp
    (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                          (with-esf-end-of-sexp
                            (live-bounds-of-preceding-sexp)))))
  (define-eval-sexp-fu-flash-command eval-defun
    (eval-sexp-fu-flash (live-bounds-of-defun))))

(live-esf-initialize-elisp)

;; cider extensions


(defun live-bounds-of-cider-last-sexp ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn cider-last-sexp to ensure highlighted
   area is identical to that which is evaluated."
  (cons (save-excursion (backward-sexp) (point)) (point)))

(defun live-esf-initialize-cider ()
  (define-eval-sexp-fu-flash-command cider-eval-last-sexp
    (eval-sexp-fu-flash (live-bounds-of-cider-last-sexp)))

  (define-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (eval-sexp-fu-flash (live-bounds-of-cider-last-sexp)))

  (define-eval-sexp-fu-flash-command cider-eval-defun-at-point
    (eval-sexp-fu-flash (let ((bounds (cider--region-for-defun-at-point)))
                          (cons (first bounds) (second bounds)))))


  (progn
    ;; Defines:
    ;; `eval-sexp-fu-cider-sexp-inner-list',
    ;; `eval-sexp-fu-cider-sexp-inner-sexp'
    ;; and the pprint variants respectively.
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-eval-sexp
      cider-eval-last-sexp)
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-pprint-eval-sexp
      cider-pprint-eval-last-sexp)))

;;; rc-misc.el ends here
