(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)))))

;; local sources
(setq el-get-sources
      '((:name ctags-update
               :type emacswiki)))

(setq my-packages
      (append
       '(;;look-and-feel
         rainbow-delimiters zenburn-theme
         rainbow-mode ;;colorize color names in buffers
         ;;useful stuff
         auto-complete icomplete+ grep+ ;;yasnippet session
         org-mode
         undo-tree smex
         ace-jump-mode
         deft
         ;;vcs
         magit magithub gist
         ;;language-specific modes
         coffee-mode js2-mode ;python-mode
         haskell-mode haskell-mode-exts shime
         clojure-mode slime paredit elein
         ess
         haml-mode sass-mode rhtml-mode yaml-mode
         inf-ruby ruby-compilation rvm rinari
         markdown-mode
         auctex)
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
 '(ctags-update-delay-seconds 30)
 '(custom-safe-themes (quote ("d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "3c221cf1a0a4172917772c71da5c4d5e1d4f98c4" "9cdf9fb94f560902b567b73f65c2ed4e5cfbaafe" default)))
 '(ecb-layout-name "left14")
 '(ecb-options-version "2.40")
 '(ecb-show-sources-in-directories-buffer (quote always))
 '(ecb-source-path (quote ("~/.emacs.d" "~/exps" "~/work/repos/yawndb" "~/work/repos/saelmon" "~")))
 '(ecb-tip-of-the-day nil)
 '(ecb-toggle-layout-sequence (quote ("left9" "left14")))
 '(ecb-tree-do-not-leave-window-after-select (quote (ecb-history-buffer-name ecb-directories-buffer-name ecb-methods-buffer-name)))
 '(ecb-windows-width 0.22))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((t (:inherit zenburn-highlight-damp)))))

(mapc (lambda (name)
        (load (concat root-dir
                      (format "rc/emacs-rc-%s" name)) t))

      '(defuns flymake flyspell ido local
        ecb tags
        markup
        org deft
        python octave erlang haskell js lisp clojure R ror
        bindings))

(load (concat root-dir "private.el"))

(add-to-list 'load-path "~/.emacs.d/extra-libs/showoff-mode")
(require 'showoff-mode)
(add-to-list 'load-path "~/.emacs.d/extra-libs/nitrogen-mode")
(require 'nitrogen-mode)

(server-start)
