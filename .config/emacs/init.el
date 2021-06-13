;;; -*- lexical-binding: t; -*-

(load-file (expand-file-name "deps.el" user-emacs-directory))

(require 'package)
(package-initialize)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(defun my/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(org-babel-load-file (expand-file-name "core.org" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))

;; end
