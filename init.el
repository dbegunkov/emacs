(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; local sources
(setq el-get-sources
      '())

(setq my-packages
      (append
       '(;;look-and-feel
         rainbow-delimiters zenburn-theme
         ;rainbow-mode ;;colorize color names in buffers
         ;;useful stuff
         auto-complete icomplete+ session grep+ ;;yasnippet
         org-mode
         undo-tree smex
         ace-jump-mode iy-go-to-char
         ;;vcs
         magit magithub gist
         ;;language-specific modes
         coffee-mode js2-mode ;python-mode
         haskell-mode haskell-mode-exts shime
         clojure-mode slime paredit elein
         ess
         markdown-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(el-get 'sync)
(el-get 'wait)

(setq
 root-dir (file-name-directory (or (buffer-file-name)
                                   load-file-name)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3c221cf1a0a4172917772c71da5c4d5e1d4f98c4" "9cdf9fb94f560902b567b73f65c2ed4e5cfbaafe" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(mapc (lambda (name)
        (load (concat root-dir
                      (format "rc/emacs-rc-%s" name)) t))


      '(defuns flymake flyspell ido local
        markup
        org deft
        python octave erlang haskell js lisp clojure R
        bindings))

(load (concat root-dir "private.el"))

(add-to-list 'load-path "~/.emacs.d/extra-libs/showoff-mode")
(require 'showoff-mode)
(add-to-list 'load-path "~/.emacs.d/extra-libs/nitrogen-mode")
(require 'nitrogen-mode)

(server-start)
