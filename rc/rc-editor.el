;;; rc-editor.el ---

;; Note
;; ----
;; A lot of stuff here was stolen from the wonderful 'Emacs Prelude'
;; project, available at 'https://github.com/bbatsov/prelude'.

(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width 4           ;; but maintain correct appearance
              case-fold-search t    ;; case INsensitive search
              default-directory "~"
              fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; take care of the whitespace
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail
                              space-before-tab
                              indentation space-after-tab)
      whitespace-line-column 80)

;; nice things
(setq next-line-add-newlines nil  ;; don't add new lines when scrolling down
      require-final-newline t     ;; end files with a newline
      mouse-yank-at-point t       ;; yank at cursor, NOT at mouse position
      kill-whole-line t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t     ;; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ;; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq-default save-place t) ;; activate it for all buffers
(setq save-place-file (local-file-name "cache/saveplace"))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (local-file-name "cache/savehist"))
(savehist-mode t)

(require 'desktop)
(setq-default desktop-missing-file-warning nil
              desktop-load-locked-desktop t
              desktop-restore-eager 0
              desktop-restore-frames 1
              desktop-path `(,(local-file-name "cache"))
              desktop-save t)
(desktop-save-mode t)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 30)
                (shell-command-history    . 50)
                tags-file-name
                register-alist))
      desktop-locals-to-save nil)
(desktop-read)

;; save recent files
(require 'recentf)
(setq recentf-save-file (local-file-name "cache/recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around nil)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; diminish keeps the modeline tidy
(require 'diminish)

;; subtle highlighting of matching parens (global-mode)
(use-package smartparens
  :ensure smartparens
  :init (progn
          (show-smartparens-global-mode t)
          (smartparens-global-mode 1))
  :config (progn
            (require 'smartparens-config)
            (setq sp-autoskip-closing-pair 'always)

            (let ((map smartparens-mode-map))
              ;; Depth changing
              (define-key map (kbd "M-<up>") #'sp-splice-sexp-killing-backward)
              (define-key map (kbd "M-<down>") #'sp-splice-sexp-killing-forward)
              ;; Barfage & Slurpage
              (define-key map (kbd "C-)")  #'sp-forward-slurp-sexp)
              (define-key map (kbd "C-}")  #'sp-forward-barf-sexp)
              (define-key map (kbd "C-(")  #'sp-backward-slurp-sexp)
              (define-key map (kbd "C-{")  #'sp-backward-barf-sexp))

            (let ((map smartparens-strict-mode-map))
              (define-key map (kbd ")") #'sp-up-sexp)
              (define-key map (kbd "]") #'sp-up-sexp)
              (define-key map (kbd "}") #'sp-up-sexp))))

;; (require 'smartparens)
;; (require 'smartparens-config)
;; (show-smartparens-global-mode t)
;; (smartparens-global-mode 1)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :config (progn
            (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

;; do not highlight the current line
(global-hl-line-mode -1)

(use-package volatile-highlights
  :ensure volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; tramp, for sudo access
(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *"
      tramp-default-method "ssh"
      tramp-temp-buffer-file-name (local-file-name "cache/tramp"))

;; helm all the things!
(use-package helm
  :ensure helm
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)

    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)

    ;; (defun pl/helm-alive-p ()
    ;;   (if (boundp 'helm-alive-p)
    ;;       (symbol-value 'helm-alive-p)))
    ;; (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

    (setq helm-M-x-fuzzy-match        t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t)

    (global-set-key (kbd "C-x b")   'helm-mini)
    (global-set-key (kbd "M-x")     'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "M-y")     'helm-show-kill-ring)

    (helm-mode 1)))

;; (use-package shackle
;;   :ensure shackle
;;   :config
;;   (progn
;;     (setq shackle-rules '(("^\*helm[ -].+\*$" :regexp t :align below :size 0.3)))
;;     (shackle-mode 1)))

(use-package popwin
  :ensure popwin
  ;; :diminish popwin-mode
  :config
  (progn
    (setq display-buffer-function 'popwin:display-buffer)

    (popwin-mode 1)

    (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
    (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

    ;; (add-hook 'helm-after-initialize-hook (lambda ()
    ;;                                         (popwin:display-buffer helm-buffer t)
    ;;                                         (popwin-mode -1)))

    ;;  Restore popwin-mode after a Helm session finishes.
    ;; (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

    (setq helm-split-window-preferred-function 'ignore)))

;; enabled auto-fill mode in text-mode and all related modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load yasnippet
(require 'yasnippet)
(require 'dropdown-list)
(add-to-list 'yas-snippet-dirs (local-file-name "snippets"))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt))

;; load flycheck
;; (use-package flycheck
;;   :ensure flycheck
;;   :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; load auto-complete
(use-package company
  :ensure company
  :config (progn
            (global-company-mode)
            (setq company-idle-delay 0.2)))

;; use TAB for completion and indentation
(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

;; (use-package auto-complete
;;   :ensure auto-complete
;;   :config (progn
;;             (add-to-list 'ac-dictionary-directories
;;                          "~/.emacs.d/ac-dict")))

;; (require 'auto-complete-config nil t)
;; (ac-config-default)
;; (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat"
;;       ac-candidate-limit 20
;;       ac-ignore-case nil)
;; (global-auto-complete-mode)

;; (add-hook 'prog-mode-hook 'auto-complete-mode)


;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (local-file-name "cache/eshel"))

;; smex, remember recently and most frequently used commands
;; (use-package smex
;;   :ensure smex
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands))
;;   :config (progn
;;           (setq smex-save-file (local-file-name "cache/.smex-items"))
;;           (smex-initialize)))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; sensible undo
(use-package undo-tree
  :ensure undo-tree
  :diminish undo-tree-mode
  :commands undo-tree
  :config (progn
            (global-undo-tree-mode 1)
            (defalias 'redo 'undo-tree-redo)))

;; my git
(use-package magit
  :ensure magit
  :commands magit-status
  :bind ("C-c g" . magit-status)
  :config (progn
            (setq magit-push-always-verify nil)
            (add-to-list 'magit-no-confirm 'stage-all-changes))
  ;; :config (setq magit-emacsclient-executable nil)
  )

;; incremental searching
(use-package anzu
  :ensure anzu
  :config (global-anzu-mode +1))

;; better grep-find
(use-package ag
  :ensure ag
  :commands ag
  :config (setq ag-highlight-search t
                ag-reuse-window t))

;; fix me already!
;; (use-package fixmee
;;   :ensure fixmee
;;   :diminish fixmee-mode
;;   :config (global-fixmee-mode 1))

;; view large files easily
(use-package vlf
  :ensure vlf
  :commands vlf
  :config (require 'vlf-setup))

;; semantic region expansion
(use-package expand-region
  :ensure expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region)
  :config (progn
            ;; see https://github.com/magnars/expand-region.el/issues/160
            (defun er--expand-region-1 ()
              "Increase selected region by semantic units.
Basically it runs all the mark-functions in `er/try-expand-list'
and chooses the one that increases the size of the region while
moving point or mark as little as possible."
              (let* ((p1 (point))
                     (p2 (if (use-region-p) (mark) (point)))
                     (start (min p1 p2))
                     (end (max p1 p2))
                     (try-list er/try-expand-list)
                     (best-start (point-min))
                     (best-end (point-max))
                     (set-mark-default-inactive nil))

                ;; add hook to clear history on buffer changes
                (unless er/history
                  (add-hook 'after-change-functions 'er/clear-history t t))

                ;; remember the start and end points so we can contract later
                ;; unless we're already at maximum size
                (unless (and (= start best-start)
                             (= end best-end))
                  (push (cons start end) er/history))

                (when (and expand-region-skip-whitespace
                           (er--point-is-surrounded-by-white-space)
                           (= start end))
                  (skip-chars-forward er--space-str)
                  (setq start (point)))

                (while try-list
                  (save-excursion
                    (ignore-errors
                      (setq mark-active nil) ; Fix by Stefan Monnier
                      (funcall (car try-list))
                      (when (and (region-active-p)
                                 (er--this-expansion-is-better start end best-start best-end))
                        (setq best-start (point))
                        (setq best-end (mark))
                        (when (and er--show-expansion-message (not (minibufferp)))
                          (message "%S" (car try-list))))))
                  (setq try-list (cdr try-list)))

                (setq deactivate-mark nil)
                (goto-char best-start)
                (set-mark best-end)

                (er--copy-region-to-register)

                (when (and (= best-start (point-min))
                           (= best-end (point-max))) ;; We didn't find anything new, so exit early
                  'early-exit))))
  )

;; better current major mode help
(use-package discover-my-major
  :ensure discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; projectile for searching in projects
(use-package projectile
  :ensure projectile
  :config (projectile-global-mode))

(use-package helm-projectile
  :ensure helm-projectile
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package helm-ag
  :ensure helm-ag
  :config (define-key projectile-mode-map (kbd "C-c p /")
          #'(lambda ()
              (interactive)
              (helm-ag (projectile-project-root)))))

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode)
  ;; :config (add-hook 'ace-jump-mode-end-hook 'golden-ratio)
  )

;;; rc-editor.el ends here
