;;; core-packages.el ---

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-user-dir (local-file-name "elpa"))

(when (not package-archive-contents)
  (package-refresh-contents))

(package-initialize)

(defvar my-start-packages
  '(dash
    ido-ubiquitous
    flx-ido
    ido-vertical-mode
    scratch
    diminish
    ;; exec-path-from-shell ;; not needed on Linux
    yasnippet
    dropdown-list
    use-package)
  "A list of packages to ensure are installed at launch.")

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my-start-packages)

(require 'use-package)


;;; core-packages.el ends here
