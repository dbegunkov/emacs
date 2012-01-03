(require 'semantic/analyze)
(provide 'semantic-analyze)
(provide 'semantic-ctxt)
(provide 'semanticdb)
(provide 'semanticdb-find)
(provide 'semanticdb-mode)
(provide 'semantic-load)

(setq stack-trace-on-error t)
;;(ecb-activate)
(ecb-byte-compile)
(custom-set-variables
 '(ecb-layout-name "left14")
 '(ecb-toggle-layout-sequence '("left9" "left14"))
 '(ecb-show-sources-in-directories-buffer 'always)
 '(ecb-tree-do-not-leave-window-after-select
   (quote (ecb-history-buffer-name
           ecb-directories-buffer-name
           ecb-methods-buffer-name)))
 '(ecb-windows-width 0.22)
 '(ecb-tip-of-the-day nil)
 '(ecb-source-path '(
                     "~/.emacs.d"
                     "~/exps"
                     "~/work/repos/yawndb"
                     "~/work/repos/saelmon"
                     "~"
                     ))
 '(ecb-options-version "2.40"))

(custom-set-faces
 '(ecb-default-highlight-face ((t (:inherit zenburn-highlight-damp)))))
