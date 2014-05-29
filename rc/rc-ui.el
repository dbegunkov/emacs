;;; rc-ui.el ---


(when (window-system)
  (let ((font-name "Source Code Pro Medium-10"))
    (when (find-font (font-spec :name font-name))
      (set-frame-font font-name))))

(set-fontset-font "fontset-default"
                  '(#x0400 . #x04ff)
                  "Dejavu Sans Mono-10")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; disable startup screen and *scratch* message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(mouse-avoidance-mode 'cat-and-mouse)

(global-linum-mode 0)   ;; no line number unless i say so ...
(blink-cursor-mode -1)  ;; ... and cut out that blinking, okay?

(setq cursor-in-non-selected-windows nil
      use-dialog-box nil)

;; stop prompting me, allright?
;; a) y is yes and n is no
(fset 'yes-or-no-p 'y-or-n-p)
;; b) i don't care if the process is running
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(use-package smart-mode-line
  :ensure smart-mode-line
  :init (progn
          (setq sml/theme 'dark)
          (sml/setup)))

(use-package color-theme-sanityinc-tomorrow
  :ensure color-theme-sanityinc-tomorrow
  :config (load-theme 'sanityinc-tomorrow-eighties))


;;; rc-ui.el ends here