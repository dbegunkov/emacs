;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; init.el --- initialization

(defvar local-dir (file-name-directory user-init-file)
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(setq custom-file (local-file-name "custom.el"))
(load custom-file t)

(load (local-file-name "core/core-packages"))
(load (local-file-name "core/core-env"))

(load (local-file-name "rc/rc-ui"))
(load (local-file-name "rc/rc-editor"))
(load (local-file-name "rc/rc-defuns"))
(load (local-file-name "rc/rc-languages"))
(load (local-file-name "rc/rc-markup"))
(load (local-file-name "rc/rc-flyspell"))
(load (local-file-name "rc/rc-global-bindings"))

(load (local-file-name "private.el") t)

(when-osx
    (toggle-frame-maximized))

;;; init.el ends here
