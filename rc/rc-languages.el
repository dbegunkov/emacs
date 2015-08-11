;;; rc-languages.el ---


(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'turn-on-whitespace)

;; Text

(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; Python

(use-package virtualenvwrapper
  :ensure virtualenvwrapper
  :config (progn
            (venv-initialize-interactive-shells) ;; interactive shell support
            (venv-initialize-eshell)             ;; eshell support
            (setq venv-location "~/.virtualenvs/")

            (defun venvwrap-projectile-hook ()
              (let ((project-name (projectile-project-name))
                    (venvs (venv-list-virtualenvs)))
                (when (string-match project-name venvs)
                  (venv-workon project-name))))

            (defun venvwrap-modeline-hook ()
              (setq-default mode-line-format
                            (nconc mode-line-format
                                   '(:eval (concat "venv:" venv-current-name)))))

            (add-hook 'python-mode-hook 'venvwrap-projectile-hook)
            ;;(add-hook 'venv-postactivate-hook 'venvwrap-modeline-hook)
))

(use-package anaconda-mode
  :ensure anaconda-mode
  :diminish anaconda-mode
  :config (progn
            (add-hook 'python-mode-hook 'anaconda-mode)
            (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package company-anaconda
  :ensure company-anaconda
  :config (progn
            (add-to-list 'company-backends 'company-anaconda)))

;; (use-package python-mode
;;   :ensure python-mode
;;   :mode ("\\.py\\'" . python-mode)
;;   :commands python-mode
;;   :config (progn
;;             (add-hook 'python-mode-hook
;;                       (lambda () (run-hooks 'prog-mode-hook)))
;;             (add-hook 'python-mode-hook 'anaconda-mode)
;;             (add-hook 'python-mode-hook 'eldoc-mode)
;;             (add-hook 'python-mode-hook
;;                       (lambda ()
;;                         ;; See https://github.com/company-mode/company-mode/issues/105
;;                         ;; for details on this nasty bug.
;;                         (remove-hook 'completion-at-point-functions
;;                                      'py-shell-complete t)
;;                         (subword-mode +1)
;;                         (electric-indent-mode -1)))))

;; (use-package cython-mode
;;   :ensure cython-mode
;;   :commands cython-mode
;;   :config (add-hook 'cython-mode-hook
;;                     (lambda ()
;;                       ;; same bug for cython, damit!
;;                       (remove-hook 'completion-at-point-functions
;;                                    'py-shell-complete t))))

;; Erlang

(require 'em-glob)

(setq erlang-root-dir
      (if (eq system-type 'gnu/linux)
          "/usr/local/lib/erlang"
        "/usr/local/lib/erlang"))

(defun directory-files-glob (path)
  (directory-files (file-name-directory path)
                   t
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun directory-any-file-glob (path)
  (car (directory-files-glob path)))

(when (file-exists-p erlang-root-dir)
  (add-to-list 'load-path (concat
                           (file-name-as-directory
                            (directory-any-file-glob
                             (concat erlang-root-dir "/lib/tools-*")))
                           "emacs"))

  (require 'erlang-start))

;; Haskell

;; (use-package haskell-mode
;;   :ensure haskell-mode
;;   :commands haskell-mode
;;   :config (progn
;;             (require 'inf-haskell)
;;             (require 'haskell-compile)
;;             (require 'haskell-checkers)
;;             (require 'haskell-navigate-imports)

;;             (bind-keys :map haskell-mode-map
;;                        ("C-c C-c" . haskell-compile)
;;                        ("M-[" . haskell-navigate-imports)
;;                        ("M-]" . haskell-navigate-imports-return))

;;             (add-hook 'haskell-mode-hook
;;                       '(lambda ()
;;                          (subword-mode +1)
;;                          (haskell-doc-mode 1)))))

;; (use-package hi2
;;   :ensure hi2
;;   :config (progn
;;             (add-hook 'haskell-mode-hook
;;                       '(lambda ()
;;                          (setq tab-width 4
;;                                hi2-layout-offset 4
;;                                hi2-left-offset 4
;;                                hi2-ifte-offset 4)

;;                          ;; (haskell-indent-mode -1)
;;                          (hi2-mode)))))

;; (use-package ghci-completion
;;   :ensure ghci-completion
;;   :config (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))


;; ;; OCaml

;; (when (executable-find "opam")
;;   (add-to-list 'load-path
;;                (concat
;;                 (replace-regexp-in-string
;;                  "\n$" ""
;;                  (shell-command-to-string "opam config var share"))
;;                 "/emacs/site-lisp"))

;;   (let* ((opam-prefix
;;           (substring (shell-command-to-string "opam config var prefix") 0 -1)))
;;     (with-temp-buffer
;;       (insert (shell-command-to-string
;;                (concat opam-prefix
;;                        "/bin/ocp-edit-mode emacs -load-global-config")))
;;       (eval-buffer)))

;;   (require 'ocp-indent)
;;   ;; only supports autocomplete :(
;;   ;; (require 'ocp-index)
;;   (require 'tuareg)
;;   (setq ocp-indent-config "with_never=true"))


;; ;; Coffee

;; (use-package coffee-mode
;;   :ensure coffee-mode
;;   :commands coffee-mode
;;   :config (add-hook 'coffee-mode-hook
;;                   '(lambda ()
;;                      (set (make-local-variable 'tab-width) 2)
;;                      (setq coffee-args-compile '("-c", "--bare")
;;                            coffee-debug-mode t)

;;                      ;; Compile '.coffee' files on every save
;;                      (and (file-exists-p (buffer-file-name))
;;                           (file-exists-p (coffee-compiled-file-name))
;;                           (coffee-cos-mode t)))))

;; ;; C, C++

;; (use-package cc-mode
;;   :config (add-hook 'c-mode-common-hook
;;                   '(lambda ()
;;                      (local-set-key (kbd "RET") 'newline-and-indent)
;;                      (setq c-default-style "linux"
;;                            c-basic-offset 4)
;;                      (c-set-offset 'substatement-open 0))))

;; R

(use-package ess-site
  :ensure ess
  :commands R
  :config (progn
            ;; TODO: why doesn't use-package require it for us?
            (require 'ess-site)

            (setq ess-eval-visibly-p nil
                  ess-use-tracebug t
                  ess-use-auto-complete t
                  ess-help-own-frame 'one
                  ess-ask-for-ess-directory nil)
            (setq-default ess-dialect "R")
            (ess-toggle-underscore nil)))

;; Octave

(use-package octave-mode
  :mode "\\.m$")


;; Elisp

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun recompile-elc-on-save ()
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'recompile-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

(use-package eldoc
  :diminish eldoc-mode
  :config (progn (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)))

(use-package eval-sexp-fu
  :ensure eval-sexp-fu)


;; Clojure & ClojureScript

;; borrowed from emacs-live
;; Specify the print length to be 100 to stop infinite sequences killing
;; things. This might be dangerous for some people relying on
;; *print-length* being larger. Consider a work around
(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(use-package typed-clojure-mode
  :ensure typed-clojure-mode
  :diminish typed-clojure-mode)

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

(use-package clojure-mode
  :ensure clojure-mode
  :mode "\\.s?cljx?\\|boot$"
  :config (progn
            (put-clojure-indent 'this-as 1)
            (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
            (add-hook 'clojure-mode-hook 'typed-clojure-mode)
            (add-hook 'clojure-mode-hook 'subword-mode)
            (add-hook 'clojure-mode-hook
                      '(lambda ()
                         (local-set-key (kbd "RET") 'electrify-return-if-match)))))

;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(when (not (package-installed-p 'cider))
      (package-install 'cider))
(use-package cider
  :ensure cider
  :config (progn
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
            (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
            (add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)
            (add-hook 'cider-repl-mode-hook 'subword-mode)
            (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
            ;; (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
            (setq nrepl-hide-special-buffers t)
            (setq cider-popup-stacktraces nil)
            ;; (setq cider-repl-popup-stacktraces nil)
            (setq cider-repl-use-clojure-font-lock t)
            (live-esf-initialize-cider)
            ))

(use-package clojure-cheatsheet
  :ensure clojure-cheatsheet)

;;; rc-languages.el ends here
