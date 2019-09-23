
;; this is required when symlink project outside GOPATH into it
(setq vc-follow-symlinks nil)

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :ensure-system-package
  ((goimports . "go get -u golang.org/x/tools/cmd/goimports")
   (godef . "go get -u github.com/rogpeppe/godef"))
  :init
  (setq gofmt-command "goimports"
        indent-tabs-mode t)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c C-c" . compile)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-k" . godoc)
              ("M-." . godef-jump)))

(use-package go-eldoc
  :after go-mode
  :ensure-system-package (gocode . "go get -u github.com/stamblerre/gocode")
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))


;; (use-package company-go
;;   :after (company go-eldoc)
;;   :config
;;   (add-hook 'go-mode-hook (lambda ()
;;                           (set (make-local-variable 'company-backends) '(company-go))
;;                           (company-mode)))
;; )

(use-package go-errcheck
  :after go-mode
  :ensure-system-package (errcheck . "go get -u github.com/kisielk/errcheck")
  :bind (:map go-mode-map
              ("C-c C-e" . go-errcheck)))

(use-package go-imenu
  :ensure-system-package (go-outline . "go get -u github.com/lukehoban/go-outline")
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
  :after go-mode
  :config
  (setq go-playground-basedir "~/code/go/src/playground")
  (my/global-map-and-set-key "C-R" 'go-playground-exec)
  )

(use-package go-rename
  :after go-mode
  :ensure-system-package (gorename . "go get -u golang.org/x/tools/cmd/gorename")
  :bind (:map go-mode-map
              ("C-c C-r" . go-rename)
              ))

;; https://github.com/abrochard/emacs-config/blob/master/configuration.org#go
