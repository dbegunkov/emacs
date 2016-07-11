;;; core-packages.el ---

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-user-dir (local-file-name "packages"))

(package-initialize)

(defvar my-start-packages
  '(dash
    ido-ubiquitous
    flx-ido
    ido-vertical-mode
    smex
    ;; scratch
    diminish
    exec-path-from-shell ;; not needed on Linux yasnippet
    dropdown-list use-package base16-theme
    color-theme-sanityinc-tomorrow)
    "A list of packages to ensure are installed at launch.")

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my-start-packages)

(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])

(require 'use-package)


;;; core-packages.el ends here
