
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq gofmt-command "goimports")

  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c C-c" . compile)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-k" . godoc)
              ;; ("C-c C-e" . go-errcheck)
              ("C-c C-r" . go-remove-unused-imports)
              ("M-." . godef-jump))
  )

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package company-go
  :after company
  :init (add-to-list 'company-backends 'company-go)
  :config
  (setq company-echo-delay 0)
  )
