(use-package smartparens
  :config (require 'smartparens-config)
  :hook ((c++-mode c-mode python-mode
                   ruby-mode js2-mode tuareg-mode
                   go-mode rust-mode) . smartparens-mode))

(use-package lsp-mode
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         ;; (js2-mode . lsp-deferred)
         )
  :ensure-system-package
  ((gopls . "go get golang.org/x/tools/gopls@latest")
   (rls . "rustup component add rls rust-analysis rust-src")
   (pyls . "pip install 'python-language-server[all]'")
   (typescript-language-server . "npm install -g typescript-language-server"))

  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-width 80
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-ignore-duplicate t)
  )

(use-package company-lsp
  :commands company-lsp
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-lsp))
                            (company-mode))))


