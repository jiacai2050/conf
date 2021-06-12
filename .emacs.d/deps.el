(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/vendor/use-package")
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))

(use-package company
  :load-path "~/.emacs.d/vendor/company-mode"
  :commands (global-company-mode)
  :init
  (global-company-mode t)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay .3
        company-begin-commands '(self-insert-command org-self-insert-command)
        company-dabbrev-downcase nil
        ;; company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-backends '((company-capf company-dabbrev-code company-dabbrev
                                         company-gtags company-etags company-keywords)
                           (company-files)))

  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-i" . company-complete-selection)))

;; magit deps
(use-package with-editor
  :load-path "~/.emacs.d/vendor/with-editor"
  :commands with-editor-export-editor
  ;; sets up the with-editor package so things that invoke $EDITOR will use the current emacs if I’m already inside of emacs
  :hook ((shell-mode eshell-mode) . with-editor-export-editor))

(use-package dash
  :load-path "~/.emacs.d/vendor/dash.el"
  :defer t)

(use-package transient
  :load-path "~/.emacs.d/vendor/transient/lisp"
  :custom (;; https://emacs.stackexchange.com/a/52002/16450
           (transient-display-buffer-action '(display-buffer-below-selected)))
  :commands (transient-define-prefix)
  :config
  (transient-bind-q-to-quit))

;; lsp-treemacs deps
(use-package dash-functional
  :load-path "~/.emacs.d/vendor/dash.el"
  :defer t)

(use-package ht
  :load-path "~/.emacs.d/vendor/ht.el"
  :defer t)

(use-package s
  :load-path "~/.emacs.d/vendor/s.el"
  :defer t)

(use-package f
  :load-path "~/.emacs.d/vendor/f.el"
  :defer t)

;; lsp-mode deps
(use-package spinner
  :load-path "~/.emacs.d/vendor/spinner.el"
  :defer t)

(use-package lv
  :load-path "~/.emacs.d/vendor/hydra"
  :defer t)

;; easy-hugo deps
(use-package request
  :load-path "~/.emacs.d/vendor/emacs-request"
  :defer t)

;; unicode-escape deps
(use-package names
  :load-path "~/.emacs.d/vendor/names"
  :defer t)

;; company-tabnine deps, other: company, dash, s, cl-lib
(use-package unicode-escape
  :load-path "~/.emacs.d/vendor/unicode-escape.el"
  :defer t)

;; evil deps
(use-package goto-chg
  :load-path "~/.emacs.d/vendor/goto-chg"
  :defer t)

;; (setq my/after-deps-loaded (current-time))
;; (message "Load deps.el cost %f seconds"
;; 		 (float-time
;; 		  (time-subtract my/after-deps-loaded my/after-early-init-loaded)))
