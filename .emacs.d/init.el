;;; -*- lexical-binding: t; -*-

(require 'package)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p '(use-package))
  (when (not (package-installed-p p))
    (package-install p)))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))

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
        mc/list-file (no-littering-expand-etc-file-name "mc-lists.el")))

;; magit dependencies
(use-package with-editor
  :commands with-editor-export-editor
  ;; sets up the with-editor package so things that invoke $EDITOR will use the current emacs if I’m already inside of emacs
  :hook ((shell-mode eshell-mode) . with-editor-export-editor)
  )
(use-package dash
  :defer t)
(use-package transient
  :custom (;; https://emacs.stackexchange.com/a/52002/16450
           (transient-display-buffer-action '(display-buffer-below-selected)))
  :config
  (transient-bind-q-to-quit))

;; lsp-treemacs deps
(use-package dash-functional
  :defer t)
(use-package f
  :defer t)
(use-package ht
  :defer t)

;; lsp-mode deps
(use-package spinner
  :defer t)
(use-package lv
  :defer t)

;; easy-hugo deps
(use-package request
  :defer t)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(let ((custom-conf-path (file-name-as-directory (expand-file-name "customizations" user-emacs-directory))))
  (add-to-list 'load-path custom-conf-path)
  (org-babel-load-file (expand-file-name "init.org" custom-conf-path))
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
