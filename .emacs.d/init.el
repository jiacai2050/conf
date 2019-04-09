;; Define package repositories
(require 'package)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                         ;; ("melpa-stable2" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
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
    smartparens
    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider
    ;; https://github.com/clojure-emacs/clj-refactor.el
    clj-refactor

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    ;; smex

    ;; https://common-lisp.net/project/slime/doc/html/Installation.html#Installation
    slime
    ;; ruby
    robe
    enh-ruby-mode
    ruby-end
    flymake-ruby
    ;; python
    elpy
    pyenv-mode
    ;; js
    nodejs-repl
    js2-mode
    ;; misc
    ;; project navigation
    projectile
    ag
    yaml-mode
    ;; https://github.com/rolandwalker/minimal-session-saver
    ;; minimal-session-saver
    ;; https://github.com/Fanael/persistent-scratch/
    persistent-scratch
    markdown-mode
    use-package
    ;; colorful parenthesis matching
    rainbow-delimiters
    ;; edit html tags like sexps
    tagedit
    evil-numbers
    expand-region
    company
    multiple-cursors
    ;; https://github.com/justbur/emacs-which-key
    which-key
    ;; https://magit.vc/manual/magit/Getting-started.html#Getting-started
    magit
    git-link
    ;; magithub
    ;; super convenient for cut & paste
    browse-kill-ring
    kill-ring-search

    ;; org
    ;; https://emacs-china.org/t/topic/440
    cnfonts
    org-bullets
    ox-gfm
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

;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;
(setq custom-file (concat user-emacs-directory ".custom.el"))

;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/35#issuecomment-36456031
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)
;; (defvar predicate nil)
;; (defvar inherit-input-method nil)

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
(load "restclient.el")

;; Langauage-specific
(load "setup-org.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-common-lisp.el")
(load "setup-ruby.el")
(load "setup-python.el")
(load "setup-go.el")
(load "setup-rust.el")
(org-babel-load-file "~/.emacs.d/customizations/misc.org")
;; https://github.com/blak3mill3r/vmd-mode
(let ((vmd-repo-dir "~/.emacs.d/vendor/vmd-mode"))
  (when (file-exists-p vmd-repo-dir)
    (add-to-list 'load-path vmd-repo-dir)
    (require 'vmd-mode)
    (global-set-key (kbd "C-c M-m") 'vmd-mode)))
;; end

