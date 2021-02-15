;;; -*- lexical-binding: t; -*-

(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("marmalada" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/"))
      )

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p '(use-package use-package-ensure-system-package))
  (when (not (package-installed-p p))
    (package-install p)))

(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t))

(defun my/map-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY\."
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

(defun my/global-map-and-set-key (key command &optional prefix suffix)
  "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
   (my/map-key key)
   (global-set-key (kbd (concat prefix key suffix)) command))

(defun my/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;;;;
;; Customization
;;;;
(setq custom-file (expand-file-name "custom.el" my/ignore-directory)
      auto-save-list-file-prefix (cond ((eq system-type 'ms-dos)
	                                    ;; MS-DOS cannot have initial dot, and allows only 8.3 names
	                                    (expand-file-name "auto-save.list/_s" my/ignore-directory))
	                                   (t
	                                    (expand-file-name "auto-save-list/.saves-" my/ignore-directory)))
      type-break-file-name (expand-file-name "type-break" my/ignore-directory)
      eshell-directory-name (expand-file-name "eshell/" my/ignore-directory)
      gamegrid-user-score-file-directory (expand-file-name "games/" my/ignore-directory)
      url-configuration-directory (expand-file-name "data/url/" my/ignore-directory)
      request-storage-directory (expand-file-name "request" my/ignore-directory)
	  ;; https://github.com/emacscollective/no-littering/blob/master/no-littering.el
      tramp-auto-save-directory (expand-file-name "tramp-auto-save/" my/ignore-directory)
      tramp-persistency-file-name (expand-file-name "tramp-persistency.el" my/ignore-directory)
      )

(defconst MAC-P      (eq system-type 'darwin))
(defconst LINUX-P    (eq system-type 'gnu/linux))
(defconst WINDOWS-P  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst BSD-P      (eq system-type 'berkeley-unix))
(defconst YT-P       (string-equal "jameshn" (getenv "USER")))
(defconst DESKTOP-P  (or
                      WINDOWS-P
                      (string-match-p "cpro.roa" (system-name))
                      ;; Check width of monitor.
                      (> (cadddr (assoc 'geometry
                                        (frame-monitor-attributes)))
                         4400)))

;; magit dependencies
(use-package with-editor
  :defer t)
(use-package dash
  :defer t)
(use-package transient
  :custom (;; https://emacs.stackexchange.com/a/52002/16450
           (transient-display-buffer-action '(display-buffer-below-selected)))
  :config
  (transient-bind-q-to-quit)
  (setq transient-history-file (expand-file-name "transient/history.el" my/ignore-directory)))

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

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(let ((custom-conf-path (file-name-as-directory (expand-file-name "customizations" user-emacs-directory))))
  (add-to-list 'load-path custom-conf-path)

  ;; Sets up exec-path-from-shell so that Emacs will use the correct
  ;; environment variables
  (load "shell-integration.el")

  ;; These customizations change the way emacs looks and disable/enable
  ;; some user interface elements
  (load "ui.el")

  ;; These customizations make editing a bit nicer.
  (load "editing.el")

  ;; These customizations make it easier for you to navigate files,
  ;; switch buffers, and choose options from the minibuffer.
  (load "navigation.el")

  ;; For editing lisps
  (load "elisp-editing.el")

  ;; settings for all langauage
  (load "setup-progn.el")

  ;; Langauage-specific
  (load "setup-org.el")
  (load "setup-clojure.el")
  (load "setup-js.el")
  (load "setup-common-lisp.el")
  (load "setup-ruby.el")
  (load "setup-python.el")
  (load "setup-go.el")
  (load "setup-rust.el")

  (org-babel-load-file (expand-file-name "misc.org" custom-conf-path)))

;; (load custom-file)
;; end
