;;; -*- lexical-binding: t; -*-

(use-package go-mode
  ;; :load-path "~/.emacs.d/vendor/go-mode"
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . my/set-go-hook)
  :init
  (setq gofmt-command "goimports"
        indent-tabs-mode t)
  (defun my/set-go-hook ()
    (setq-local before-save-hook 'gofmt-before-save))
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-f" . go-test-current-file)
              ("C-c C-t" . go-test-current-test)
              ("C-c C-p" . go-test-current-project)
              ("C-c C-b" . go-test-current-benchmark)
              ("C-x x" . go-run))
  :custom
  (go-test-verbose t))

(use-package go-playground
  :defer t
  :custom
  (go-playground-basedir "~/code/go/src/playground"))

(use-package go-rename
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-r" . go-rename)))

;; https://github.com/abrochard/emacs-config/blob/master/configuration.org#go
