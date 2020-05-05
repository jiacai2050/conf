;; Define package repositories
(require 'package)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                         ;; ("melpa-stable2" . "https://stable.melpa.org/packages/")
                         ("marmalada" . "http://elpa.emacs-china.org/marmalade/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; https://common-lisp.net/project/slime/doc/html/Installation.html#Installation
    slime

    markdown-mode
    use-package
    use-package-ensure-system-package

    ;; colorful parenthesis matching
    rainbow-delimiters
    evil-numbers
    ;; super convenient for cut & paste
    browse-kill-ring
    kill-ring-search
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(when (eq system-type 'darwin)
  (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(setq use-package-always-ensure t)

;;;;
;; Customization
;;;;
(setq custom-file (concat user-emacs-directory ".custom.el"))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; For editing lisps
(load "elisp-editing.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Langauage-specific
(load "setup-org.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-common-lisp.el")
(load "setup-ruby.el")
(load "setup-python.el")
(load "setup-go.el")
(load "setup-rust.el")

;; settings for all langauage
(load "setup-common.el")
(org-babel-load-file "~/.emacs.d/customizations/misc.org")

;; end
