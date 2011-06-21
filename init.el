;;; init.el ---


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(setq
 root-dir (file-name-directory (or (buffer-file-name)
                                   load-file-name)))


(add-to-list 'load-path root-dir)
(add-to-list 'load-path (concat root-dir "el-get/el-get"))


(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))


(setq el-get-sources
      '(el-get
        ;; look-and-feel
        (:name emacs-color-theme-solarized
               :type git
               :url "https://github.com/sellout/emacs-color-theme-solarized.git")
        (:name rainbow-delimiters :type emacswiki)
        ;; generally useful stuff
        autopair auto-complete icomplete+ session scratch
        grep+ multi-term ;;yasnippet
        ;; vcs
        ahg magit
        ;; programming languages
        coffee-mode haskell-mode python-mode django-mode js2-mode
        tuareg-mode quack
        ;; markup
        markdown-mode org-mode rainbow-mode ;; auctex
        ;; rest
        google-weather

        (:name nav
               :after (lambda ()
                        (setq nav-width 25)
                        (global-set-key (kbd "C-x C-n") 'nav)))))


(el-get 'sync)

(mapc (lambda (name)
        (load (concat root-dir
                      (format "rc/emacs-rc-%s" name)) t))
      '(defuns erlang flymake flyspell haskell ido js lisp local markup
         org python ;;yasnippet

         bindings))


;;(setq custom-file (concat root-dir "custom.el"))
;;(load custom-file t)


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("b1ff9bdbf5adc046623e2338607fa5d48f9b5da1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
