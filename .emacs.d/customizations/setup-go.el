

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq gofmt-command "goimports"
        indent-tabs-mode t)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c C-c" . compile)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-k" . godoc)
              ("C-c C-r" . go-remove-unused-imports)
              ("M-." . godef-jump)))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package go-complete
  :after company
  :config
  (add-hook 'completion-at-point-functions 'go-complete-at-point)
  )

(use-package go-errcheck
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-e" . go-errcheck)))

(use-package go-imenu
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-imenu-setup))

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-f" . go-test-current-file)
              ("C-c C-t" . go-test-current-test)
              ("C-c C-p" . go-test-current-project)
              ("C-c C-b" . go-test-current-benchmark)
              ("C-x x" . go-run)))

(use-package go-playground
  :after gotest
  )

;; https://github.com/abrochard/emacs-config/blob/master/configuration.org#go

