(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; prettify
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (1 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               'zenburn-primary-5))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (1 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               'zenburn-primary-5))))))

(add-hook 'clojure-mode-hook 'paredit-mode-enable)
(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'clojure-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'electrify-return-if-match)))

;; clojure-mode
(require 'clojure-mode)

;; paredit
(require 'paredit)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
          (defun paredit-mode-enable () (paredit-mode 1))
          (add-hook 'slime-mode-hook 'paredit-mode-enable)
          (add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
          (setq slime-protocol-version 'ignore)))

(require 'slime)
(slime-setup)
