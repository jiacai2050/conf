;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(use-package which-key
  :config (which-key-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

;; (require 'magithub)
;; (magithub-feature-autoinject t)
(use-package restclient
  :mode ("\\.api\\'" . restclient-mode))
