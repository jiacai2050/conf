;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html
(setq enable-local-variables :safe)

(use-package which-key
  :config (which-key-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-link
  :bind ("C-c g l" . git-link))

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/codes/git"))

(use-package restclient
  :mode ("\\.api\\'" . restclient-mode))

;; https://emacs.stackexchange.com/a/32554/16450
(setq epa-pinentry-mode 'loopback)
;; (setq epa-file-encrypt-to "hello@liujiacai.net")
