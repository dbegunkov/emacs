;;; rc-ui.el ---

(set-frame-font "Source Code Pro Medium-10")
;; cyr font for cyr characters, Source Code Pro doesn't support it
(set-fontset-font "fontset-default"
                  '(#x0400 . #x04ff)
                  "Dejavu Sans Mono-10")

(modify-all-frames-parameters '((fullscreen . maximized)))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; do this as fast as possible
(load-theme 'base16-eighties)
(custom-theme-set-faces
 'base16-eighties
 '(linum ((t (:background "#393939" :foreground "#e8e6df"))))
 '(company-tooltip ((t (:background "#747369" :foreground "#e8e6df"))))
 '(company-tooltip-selection ((t (:background "#a09f93" :foreground "#e8e6df"))))
 '(company-tooltip-common ((t (:background "#747369" :foreground "#66cccc"))))
 '(company-tooltip-common-selection ((t (:background "#a09f93" :foreground "#6699cc"))))
 '(company-scrollbar-bg ((t (:background "#a09f93"))))
 '(company-scrollbar-fg ((t (:background "#e8e6df"))))
 '(company-preview ((t (:background "#747369" :foreground "#e8e6df"))))
 '(company-preview-common ((t (:background "#747369" :foreground "#e8e6df")))))

(set-fringe-mode '(7 . 1))
(setq indicate-buffer-boundaries 'left)

;; disable startup screen and *scratch* message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0                               ;; scroll only when on the edge
      scroll-conservatively 100000                  ;; don't jump
      scroll-preserve-screen-position 1             ;; preserve cursor on pageup/pagedown
      mouse-wheel-scroll-amount '(1 ((shift) . 5))  ;; one lines at a time (five with shift)
      mouse-wheel-progressive-speed nil             ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                   ;; scroll window under mouse
      scroll-step 1)                                ;; keyboard scroll one line at a time

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(mouse-avoidance-mode 'cat-and-mouse)

(global-linum-mode 1)   ;; I like line numbers
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

;;; rc-ui.el ends here
