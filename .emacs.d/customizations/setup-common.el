(use-package smartparens
  :config (require 'smartparens-config)
  :hook ((c++-mode c-mode python-mode
                   ruby-mode js2-mode tuareg-mode
                   go-mode rust-mode) . smartparens-mode))

(use-package lsp-mode
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         ;; (sh-mode . lsp-deferred)
         ;; (js2-mode . lsp-deferred)
         )
  :ensure-system-package
  ((gopls . "go get golang.org/x/tools/gopls@latest")
   (rls . "rustup component add rls rust-analysis rust-src")
   (pyls . "pip install 'python-language-server[all]'")
   (typescript-language-server . "npm install -g typescript-language-server")
   ;; (bash-language-server . "npm install -g bash-language-server")
   )
  :commands (lsp lsp-deferred)
  :config
  (add-hook 'before-save-hook 'lsp-format-buffer)
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-n" . lsp-find-references)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-max-width 80
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-ignore-duplicate t)
  :bind (:map lsp-ui-mode-map
              ("C-c l" . lsp-ui-imenu))
  )

(use-package company-lsp
  :commands company-lsp
  :config
  (add-hook 'lsp-mode-hook (lambda ()
                             (push 'company-lsp company-backends))))

