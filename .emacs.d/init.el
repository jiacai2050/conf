;;; -*- lexical-binding: t; -*-

(require 'package)

(load-file (expand-file-name "deps.el" user-emacs-directory))

(package-initialize)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(defun my/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;;;;
;; Customization
;;;;
(use-package no-littering
  :load-path "~/.emacs.d/vendor/no-littering"
  :config
  ;; don't forget the slash at the end of your string
  ;; https://emacs.stackexchange.com/a/17214/16450
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-var-file-name "custom.el")
        persistent-scratch-save-file (no-littering-expand-var-file-name
                                      (if (display-graphic-p)
                                          "scratch-gui.el"
                                        "scratch-terminal.el"))
        mc/list-file (no-littering-expand-etc-file-name "mc-lists.el"))

  (defun my/generate-autoloads (pkg-name &rest dirs)
    (let ((generated-autoload-file (no-littering-expand-var-file-name (format "%s-autoloads.el" pkg-name))))
      (unless (file-exists-p generated-autoload-file)
        (apply 'update-directory-autoloads dirs))
      (load-file generated-autoload-file))))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(let ((custom-conf-path (file-name-as-directory (expand-file-name "customizations" user-emacs-directory))))
  (add-to-list 'load-path custom-conf-path)
  (org-babel-load-file (expand-file-name "core.org" custom-conf-path))
  ;; settings for all langauage
  (load "setup-progn.el")
  ;; Langauage-specific
  (load "setup-clojure.el")
  (load "setup-js.el")
  ;; (load "setup-common-lisp.el")
  ;; (load "setup-ruby.el")
  (load "setup-python.el")
  (load "setup-go.el")
  (load "setup-rust.el")

  (when (file-exists-p custom-file)
	(load-file custom-file)))

;; end
